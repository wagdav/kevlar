{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Kevlar where

import           Control.Monad
import           Data.List                      ( stripPrefix
                                                , filter
                                                )
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
import qualified Kevlar.Nomad                  as Nomad

-- Oracle
newtype ContainerId = ContainerId String
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult ContainerId = Maybe String

newtype GitHash = GitHash ()
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult GitHash = String

newtype PassSecret = PassSecret String
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult PassSecret = Maybe String

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

-- Look up a specified secret using the password manager 'pass'
oraclePass = addOracle $ \(PassSecret passName) -> do
  (Exit c, Stdout out) <- quietly $ cmd ["pass", "show", passName]
  return $ case c of
    ExitSuccess   -> Just (strip out)
    ExitFailure _ -> Nothing

gitHash = askOracle $ GitHash ()

rulesOracle = do
  oracleImage
  oracleVersion
  oraclePass

mkRules :: Step -> Rules ()
mkRules (DockerImage name context needs) = build name %> \out -> do
  v         <- gitHash

  -- try to find the volume that contains the referenced context
  artifacts <- getInputArtifact v needs
  let context' = fromJust $ findPathInVolumes context (volumes artifacts)

  getDirectoryFiles context' ["//*"]
  withTempFile $ \iidfile -> do
    cmd_ ["docker", "build", "--iidfile", iidfile, context']
    imageId <- liftIO $ hashValue <$> readFile iidfile
    writeFileChanged
      out
      (show $ mempty { dockerImage = Last $ Just (name, imageId) })

mkRules (Source name src) = build name %> \out -> do
  v    <- gitHash
  --path <- if src == "." then liftIO $ makeAbsolute src else return src
  path <- if src == "." then liftIO $ makeAbsolute src else return src

  writeFileChanged
    out
    ( show
    $ mempty { artifactSources = [("github.com/wagdav/kevlar", "local/src")], volumes = [(name, path)] }
    )

mkRules (Params name e) = build name %> \out ->
  writeFileChanged out (show $ mempty { artifactParameters = Map.toList e })

mkRules (Secrets name e) = build name %> \out ->
  writeFileChanged out (show $ mempty { artifactSecrets = Map.toList e })

mkRules (Script name script caches needs) = build name %> \out -> do
  v         <- gitHash
  artifacts <- getInputArtifact v needs

  --need [fromJust $ findPathInVolumes script (volumes artifacts)]

  volCaches <- liftIO $ mapM
    makeAbsolute
    [ "_build" </> "caches" </> show i | i <- [1 .. length caches] ]
  liftIO $ mapM (createDirectoryIfMissing True) volCaches

  let (imageName, imageId) = fromJust . getLast $ dockerImage artifacts
  putNormal $ unwords ["Executing", script, "in", imageName]

  outputHost <- liftIO $ makeAbsolute $ stepOutput v name
  liftIO $ createDirectoryIfMissing True outputHost

  secrets <-
    mapM
        (\(name, path) -> do
          value <- askOracle (PassSecret path)
          unless (isJust value) $ fail ("Couldn't find the secret: " ++ path)
          return (name, fromJust value)
        )
      $ artifactSecrets artifacts

  let workdir = "/tmp/kevlar"
  let output  = workdir </> "output"
  let env =
        artifactParameters artifacts
          ++ [ ("HOME"          , workdir)
             , ("KEVLAR_OUTPUT" , output)
             , ("KEVLAR_VERSION", v)
             ]
          ++ secrets

  uid <- liftIO getEffectiveUserID
  gid <- liftIO getEffectiveGroupID

  quietly $ cmd_ ["docker", "tag", imageId, imageName ++ ":" ++ v]

  withTempDir $ \wkHost -> do
    let volArgs = concat
          [ volume wkHost workdir ReadWrite
          , concat
            [ volume hostPath (workdir </> name) ReadWrite
            | (name, hostPath) <- volumes artifacts
            ]
          , concat
            [ volume vol (makeAbsoluteIfRelative workdir c) ReadWrite
            | (vol, c) <- zip volCaches caches
            ]
          , volume outputHost output ReadWrite
          ]
--    quietly $ cmd_ [Env env] $ concat
--      [ ["docker", "run", "--rm"]
--      , ["--user", show uid ++ ":" ++ show gid]
--      , ["--workdir", workdir]
--      , concat [ ["--env", e] | (e, _) <- env ]
--      , volArgs
--      , [imageId, workdir </> script]
--      ]

    liftIO $ Nomad.writeJob "build.json" $ Nomad.mkJob
      name
      (imageName ++ ":" ++ v)
      script
      [] --(filter (/= "--volume") volArgs)
      "/local"
      (artifactSources artifacts)

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
volume local remote opt = ["--volume", local ++ ":" ++ remote]

-- Try to find the volume that contains the referenced path
findPathInVolumes :: FilePath -> [Volume] -> Maybe FilePath
findPathInVolumes p volumes =
  (</> dropDirectory1 p) <$> lookup (takeDirectory1 p) volumes

makeAbsoluteIfRelative base path =
  if isRelative path then base </> path else path

-- build parameters
build name = "_build" </> ".kevlar-work" </> name ++ ".done"
stepOutput version name = "_build" </> "artifacts" </> version </> name
