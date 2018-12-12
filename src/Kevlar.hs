{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Kevlar where

import           Control.Monad                  ( forM )
import           Data.List                      ( partition )
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.Command
import           Development.Shake.FilePath
import           System.Directory
import           System.Exit

import           Kevlar.Pipeline

-- Oracle
newtype ContainerId = ContainerId String
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult ContainerId = Maybe String

newtype GitHash = GitHash ()
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult GitHash = String

strip = filter (/= '\n')

-- oracles
oracleImage = addOracle $ \(ContainerId name) -> do
  (Exit c, Stdout out) <- quietly
    $ cmd ["docker", "inspect", "--format={{ .Id }}", name]
  return $ case c of
    ExitSuccess   -> Just (strip out)
    ExitFailure _ -> Nothing

oracleVersion = addOracle $ \GitHash{} -> do
  Stdout stdout <- quietly $ cmd "git describe --always --dirty"
  return $ strip stdout

gitHash = askOracle $ GitHash ()

rulesOracle = do
  oracleImage
  oracleVersion

mkRules :: Step -> Rules ()
mkRules (Step name (Image context)) = do
  build name %> \out -> do
    let v = version out
    need [context </> "Dockerfile"]
    askOracle (ContainerId (name ++ ":" ++ v))
    quietly $ cmd_ ["docker", "build", "--tag", name ++ ":" ++ v, context]
    writeFileChanged out v

  phony name $ do
    version <- gitHash
    need [done version name]

mkRules (Step name (Script script platform artifacts)) = do
  build name %> \out -> do
    let v                     = version out
    let (selfDeps, otherDeps) = partition thisRepo artifacts
    need $ done v platform : map (done v . artifactSource) otherDeps
    putNormal $ unwords ["Executing", script, "in", platform]

    here    <- liftIO $ makeAbsolute "."
    output  <- liftIO $ makeAbsolute $ stepOutput v name

    volSelf <- liftIO
      $ mapM (makeAbsolute . stepOutput v . artifactSource) selfDeps
    volOthers <- liftIO
      $ mapM (makeAbsolute . stepOutput v . artifactSource) otherDeps

    quietly $ cmd_ $ concat
      [ ["docker", "run", "--rm"]
      , workdir "/tmp"
      , concat
        [ volume here ("/tmp" </> artifactDestination n) ReadWrite
        | (vol, n) <- zip volSelf selfDeps
        ]
      , concat
        [ volume vol ("/tmp" </> artifactDestination n) ReadOnly
        | (vol, n) <- zip volOthers otherDeps
        ]
      , volume output "/tmp/output" ReadWrite
      , [platform ++ ":" ++ v, "/tmp" </> script]
      ]

    writeFileChanged out v

  phony name $ do
    version <- gitHash
    need [done version name]
  where workdir path = ["--workdir", path]

data VolumeOption
  = ReadOnly
  | ReadWrite
 deriving (Eq, Show)

volumeOption :: VolumeOption -> String
volumeOption ReadOnly  = "ro"
volumeOption ReadWrite = "rw"

volume :: FilePath -> FilePath -> VolumeOption -> [String]
volume local remote opt =
  ["--volume", local ++ ":" ++ remote ++ ":" ++ volumeOption opt]

-- build parameters
buildVersion v f = "_build" </> v </> f
done v name = buildVersion v $ name </> "done"
stepOutput v name = buildVersion v $ name </> "output"
build = done "*"

version = takeDirectory1 . dropDirectory1
