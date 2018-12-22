{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Kevlar where

import           Control.Monad
import           Data.List                      ( stripPrefix )
import           Data.Maybe
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.Command
import           Development.Shake.FilePath
import qualified Data.Map.Strict               as Map
import           System.Directory               ( createDirectoryIfMissing
                                                , makeAbsolute
                                                )
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

hashValue :: String -> String
hashValue x = strip $ fromMaybe x (stripPrefix "sha256:" x)

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
  v <- gitHash

  -- try to find the volume that contains the referenced context
  artifacts <- getInputArtifact v needs
  let base = lookup (takeDirectory1 context) (volumes artifacts)
  unless (isJust base) $ fail "Cannot find the specified context"
  let context' = fromJust base </> dropDirectory1 context

  getDirectoryFiles context' ["//*"]
  withTempFile $ \iidfile -> do
    cmd_ ["docker", "build", "--iidfile", iidfile, context']
    imageId <- liftIO $ hashValue <$> readFile iidfile
    writeFileChanged
      out
      (show $ mempty { dockerImage = Last $ Just (name, imageId) })

mkRules (Source name src) = build name %> \out -> do
  v    <- gitHash
  path <- if src == "."
    then liftIO $ makeAbsolute src
    else do
      repo   <- liftIO $ makeAbsolute $ stepOutput v name
      exists <- doesDirectoryExist repo
      if exists
        then do
          -- Assume that this is directory is already a git checkout.  Refresh
          -- the repository by following the instructions from
          -- https://stackoverflow.com/q/2411031
          quietly $ cmd_ [Cwd repo] ["git", "fetch"]
          quietly $ cmd_ [Cwd repo] ["git", "reset", "origin/master"]
          quietly $ cmd_ [Cwd repo] ["git", "checkout", "master"]
        else cmd_ ["git", "clone", "--recursive", src, repo]
      return repo

  writeFileChanged out (show $ mempty { volumes = [(name, path)] })

mkRules (Environment name e) = build name
  %> \out -> writeFileChanged out (show $ mempty { envVars = Map.toList e })

mkRules (Secrets name e) = build name %> \out ->
  writeFileChanged out (show $ mempty { artifactSecrets = Map.toList e })

mkRules (Script name script caches needs) = build name %> \out -> do
  v         <- gitHash
  artifacts <- getInputArtifact v needs

  volCaches <- liftIO $ mapM
    makeAbsolute
    [ "_build" </> "caches" </> show i | i <- [1 .. length caches] ]
  liftIO $ mapM (createDirectoryIfMissing True) volCaches

  let (imageName, imageId) = fromJust . getLast $ dockerImage artifacts
  putNormal $ unwords ["Executing", script, "in", imageName]

  outputHost <- liftIO $ makeAbsolute $ stepOutput v name
  liftIO $ createDirectoryIfMissing True outputHost

  let workdir = "/tmp/kevlar"
  let output  = workdir </> "output"
  let env = envVars artifacts ++ [("HOME", workdir), ("KEVLAR_OUTPUT", output)]

  uid <- liftIO getEffectiveUserID
  gid <- liftIO getEffectiveGroupID

  cmd_ ["docker", "tag", imageId, imageName ++ ":" ++ v]
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
    , [imageId, workdir </> script]
    ]

  writeFileChanged out (show $ mempty { volumes = [(name, outputHost)] })

getInputArtifact :: String -> [String] -> Action Artifact
getInputArtifact version names = do
  contents <- mapM (readFile' . build) names
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
build name = "_build" </> ".kevlar-work" </> name ++ ".done"
stepOutput version name = "_build" </> "artifacts" </> version </> name
