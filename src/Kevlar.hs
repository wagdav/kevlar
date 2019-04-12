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

import qualified Kevlar.Param as Param
import qualified Kevlar.Image as Image
import Kevlar.Need
import qualified Kevlar.Step as Step
import Kevlar.Volume

inTemporaryDirectory ::
     [(FilePath, FilePath)] -- ^ input "volumes"
  -> String -- ^ shell
  -> String -- ^ script to execute
  -> [(String, String)]  -- ^ environment variables
  -> Action ()
inTemporaryDirectory inputs shell script params =
  withTempDir $ \workDir -> do
    let env = [("HOME", workDir), ("KEVLAR_OUTPUT", workDir </> "output")] ++ params
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
inDockerContainer imageName imageLoad volumes shell script params = do
  uid <- liftIO getEffectiveUserID
  gid <- liftIO getEffectiveGroupID
  let workDir = "/tmp/build"
  let env = [("HOME", workDir), ("KEVLAR_OUTPUT", workDir </> "output")] ++ params
  maybe
    (return ())
    (\image -> inTemporaryDirectory volumes "sh" ("docker load -i " ++ image) params)
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

mkRules :: Step.Step -> Rules ()
mkRules (Step.Step name shell script image needs caches params) =
  phony stepName $ do
    volInputs <- V.mapM toVolume needs
    volOutput <- localVolume (stepOutput stepName) "output"
    volCaches <- V.mapM localCache caches'
    let volumes = [volOutput] ++ V.toList volInputs ++ V.toList volCaches
    maybe
      (inTemporaryDirectory volumes scriptShell (T.unpack script) params')
      (\image ->
         inDockerContainer
           (T.unpack $ Image.name image)
           (T.unpack <$> Image.load image)
           volumes
           scriptShell
           (T.unpack script)
           params')
      image
  where
    scriptShell = T.unpack shell
    stepName = T.unpack name
    caches' = V.map T.unpack caches
    params' = V.toList $ V.map (\p -> (T.unpack $ Param.name p, T.unpack $ Param.value p)) params

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
