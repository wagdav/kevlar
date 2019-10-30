{-# LANGUAGE TypeFamilies #-}

module Kevlar where

import Control.Arrow ((***))
import Control.Monad
import qualified Data.Text as T
import qualified Data.Vector as V
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import System.Directory (createDirectoryIfMissing, makeAbsolute)
import System.Posix.Files (createSymbolicLink)
import System.Posix.User (getEffectiveGroupID, getEffectiveUserID)

import qualified Kevlar.EnvVar as EnvVar
import Kevlar.Need
import qualified Kevlar.Step as Step
import qualified Kevlar.Action as Action
import Kevlar.Volume

inTemporaryDirectory ::
     [(FilePath, FilePath)] -- ^ input "volumes"
  -> String -- ^ shell
  -> String -- ^ script to execute
  -> [(String, String)]  -- ^ environment variables
  -> Action ()
inTemporaryDirectory inputs shell script envVars =
  withTempDir $ \workDir -> do
    let env = [("HOME", workDir), ("KEVLAR_OUTPUT", workDir </> "output")] ++ envVars
    forM_
      inputs
      (\(target, linkName) ->
         liftIO $ createSymbolicLink target (workDir </> linkName))
    cmd_
      [Stdin script, AddEnv "KEVLAR_OUTPUT" (workDir </> "output"), Cwd workDir]
      shell

inDockerContainer ::
     String -- ^ image name
  -> Maybe String -- ^ path of the docker image
  -> [(FilePath, FilePath)]
  -> String -- ^ shell
  -> String -- ^ script to execute
  -> [(String, String)]  -- ^ environment variables
  -> Action ()
inDockerContainer imageName imageLoad volumes shell script envVars = do
  uid <- liftIO getEffectiveUserID
  gid <- liftIO getEffectiveGroupID
  let workDir = "/tmp/build"
  let env = [("HOME", workDir), ("KEVLAR_OUTPUT", workDir </> "output")] ++ envVars
  maybe
    (return ())
    (\image -> inTemporaryDirectory volumes "sh" ("docker load -i " ++ image) envVars)
    imageLoad
  withTempDir $ \wkHost
    {- Create the container's working directory already on the host so that it
     will be owned by the host user. -}
   -> do
    liftIO $ createDirectoryIfMissing True (wkHost </> takeFileName workDir)
    cmd_ [Stdin script, Env env] $
      concat
        [ ["docker", "run", "--rm", "-i"]
        , ["--user", show uid ++ ":" ++ show gid]
      -- environment variables
        , concat [["--env", e] | (e, _) <- env]
      -- working directory within the container
        , volume
            (HostPath wkHost)
            (ContainerPath $ takeDirectory workDir)
            ReadWrite
      -- volumes
        , concat
            [ volume (HostPath p) (ContainerPath $ workDir </> name) ReadWrite
            | (p, name) <- volumes
            ]
      -- container starts in this directory
        , ["--workdir", workDir]
        , [imageName]
        ]

mkRules :: T.Text -> Step.Step -> Rules ()
mkRules src (Step.Step name (Step.StepDef action requires)) =
  phony stepName $ do
    need $ V.toList (V.map T.unpack requires)
    volInputs <- V.mapM toVolume needs
    volOutput <- localVolume (stepOutput stepName) "output"
    volCaches <- V.mapM localCache caches'
    let volumes = [volOutput] ++ V.toList volInputs ++ V.toList volCaches
    maybe
      (inTemporaryDirectory volumes scriptShell (T.unpack script) envVars')
      (\image ->
         inDockerContainer
           (T.unpack image)
           (T.unpack <$> load)
           volumes
           scriptShell
           (T.unpack script)
           envVars')
      image
  where
    (Action.Action shell script image load needs caches envVars) = action src
    scriptShell = T.unpack shell
    stepName = T.unpack name
    caches' = V.map T.unpack caches
    envVars' = V.toList $ V.map (\(EnvVar.EnvVar a b) -> (T.unpack a, T.unpack b)) envVars

    toVolume :: Need -> Action (FilePath, String)
    toVolume (Output name) = do
      let name' = T.unpack name
      srcAbs <- liftIO $ makeAbsolute (stepOutput name')
      need [name']
      return (srcAbs, name')
    toVolume (Fetch src dst) = return (T.unpack src, T.unpack dst)

    localVolume :: FilePath -> String -> Action (FilePath, String)
    localVolume path volName = do
      hostAbsPath <- liftIO $ makeAbsolute path
      liftIO $ createDirectoryIfMissing True hostAbsPath
      return (hostAbsPath, volName)

    localCache :: FilePath -> Action (FilePath, String)
    localCache name = localVolume (cache stepName name) name

stepOutput :: String -> FilePath
stepOutput name = "_build" </> "artifacts" </> name

cache :: String -> FilePath -> FilePath
cache stepName cacheName = "_build" </> "caches" </> stepName </> cacheName
