{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Kevlar where

import           Control.Monad
import           Data.Maybe
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.Command
import           Development.Shake.FilePath
import qualified Data.Map.Strict               as Map
import           System.Directory
import           System.Exit
import           System.Posix.User              ( getEffectiveUserID
                                                , getEffectiveGroupID
                                                )

import           Kevlar.Artifact
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
mkRules (DockerImage name context needs) = build name %> \out -> do
  let v = version out

  -- try to find the volume that contains the referenced context
  artifacts <- getInputArtifact v needs
  let base = lookup (takeDirectory1 context) (volumes artifacts)
  unless (isJust base) $ fail "Cannot find the specified context"
  let context' = fromJust base </> dropDirectory1 context

  getDirectoryFiles context' ["//*"]
  askOracle (ContainerId (name ++ ":" ++ v))
  quietly $ cmd_ ["docker", "build", "--tag", name ++ ":" ++ v, context']
  writeFileChanged out (show $ mempty { dockerImage = Last (Just name) })

mkRules (Source name src) = build name %> \out -> do
  path <- liftIO $ makeAbsolute src
  writeFileChanged out (show $ mempty { volumes = [(name, path)] })

mkRules (Environment name e) = build name
  %> \out -> writeFileChanged out (show $ mempty { envVars = Map.toList e })

mkRules (Script name script caches needs) = build name %> \out -> do
  let v = version out
  artifacts <- getInputArtifact v needs

  volCaches <- liftIO $ mapM
    makeAbsolute
    [ "_build" </> "caches" </> show i | i <- [1 .. length caches] ]
  liftIO $ mapM (createDirectoryIfMissing True) volCaches

  let platform = fromJust . getLast $ dockerImage artifacts
  putNormal $ unwords ["Executing", script, "in", platform]

  outputHost <- liftIO $ makeAbsolute $ stepOutput v name
  liftIO $ createDirectoryIfMissing True outputHost

  let workdir = "/tmp/kevlar"
  let output  = workdir </> "output"
  let env = envVars artifacts ++ [("HOME", workdir), ("KEVLAR_OUTPUT", output)]

  uid <- liftIO getEffectiveUserID
  gid <- liftIO getEffectiveGroupID

  withTempDir $ \wkHost -> quietly $ cmd_ [Env env] $ concat
    [ ["docker", "run", "--rm"]
    , ["--user", show uid ++ ":" ++ show gid]
    , ["--workdir", workdir]
    , concat [ ["--env", e] | (e, _) <- env ]
    , volume wkHost workdir ReadWrite
    , concat
      [ volume hostPath (workdir </> name) ReadWrite
      | (name, hostPath) <- volumes artifacts
      ]
    , concat
      [ volume vol (makeAbsoluteIfRelative workdir c) ReadWrite
      | (vol, c) <- zip volCaches caches
      ]
    , volume outputHost output ReadWrite
    , [platform ++ ":" ++ v, workdir </> script]
    ]

  writeFileChanged out (show $ mempty { volumes = [(name, outputHost)] })

getInputArtifact :: String -> [String] -> Action Artifact
getInputArtifact version names = do
  contents <- mapM (readFile' . done version) names
  return $ mconcat (map read contents)

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

makeAbsoluteIfRelative base path =
  if isRelative path then base </> path else path

-- build parameters
done version name = "_build" </> ".kevlar-work" </> version </> name ++ ".done"
stepOutput version name = "_build" </> "artifacts" </> version </> name
build = done "*"

version = takeBaseName . takeDirectory
