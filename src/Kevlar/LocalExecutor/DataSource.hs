{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Kevlar.LocalExecutor.DataSource
  ( initGlobalState,
    LocalExecutorReq (..),
    Artifact (..),
    DockerImage (..),
    RunOption (..),
  )
where

import Control.Concurrent.Async (async)
import Control.Concurrent.QSem
import Control.Exception (SomeException, bracket_, try)
import Control.Monad (void)
import Data.Hashable (Hashable, hash, hashWithSalt)
import GHC.Generics (Generic)
import Haxl.Core
import qualified Kevlar.Git as Git
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, withCurrentDirectory)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)
import System.Process (callProcess)

data DockerImage
  = Repository String
  | ImageTarGz String Artifact
  deriving (Eq, Show, Generic)

instance Hashable DockerImage

newtype Artifact = HostDir FilePath
  deriving (Eq, Show, Generic)

instance Hashable Artifact

data RunOption
  = Image DockerImage
  | Need Artifact FilePath
  | Cache FilePath
  | Environment [(String, String)]
  | Secret String
  deriving (Eq, Show, Generic)

instance Hashable RunOption

-- | Data source IO operations
data LocalExecutorReq a where
  Clone :: String -> LocalExecutorReq Artifact
  LocalExec :: String -> [String] -> [RunOption] -> LocalExecutorReq Artifact

-- | Haxl data source boilderplate
instance Hashable (LocalExecutorReq a) where
  hashWithSalt s (Clone src) = hashWithSalt s (0 :: Int, src)
  hashWithSalt s (LocalExec cmd args opts) = hashWithSalt s (1 :: Int, cmd, args, opts)

deriving instance Eq (LocalExecutorReq a)

deriving instance Show (LocalExecutorReq a)

instance ShowP LocalExecutorReq where showp = show

instance DataSourceName LocalExecutorReq where
  dataSourceName _ = "LocalExecutor"

-- | Datasource state
instance StateKey LocalExecutorReq where
  data State LocalExecutorReq
    = LocalExecutorState
        { semaphore :: QSem,
          workDir :: FilePath
        }

initGlobalState threads workDir = do
  sem <- newQSem threads
  return LocalExecutorState
    { semaphore = sem,
      workDir = workDir
    }

-- | data fetch implementation
instance DataSource u LocalExecutorReq where
  fetch = localExecutorFetch

localExecutorFetch :: State LocalExecutorReq -> Flags -> u -> PerformFetch LocalExecutorReq
localExecutorFetch LocalExecutorState {..} _flags _user =
  BackgroundFetch $ mapM_ (fetchAsync semaphore workDir)

fetchAsync sem workDir (BlockedFetch req rvar) =
  void $ async $ bracket_ (waitQSem sem) (signalQSem sem) $ do
    e <- Control.Exception.try $ fetchLocalExecutorReq workDir req
    case e of
      Left ex -> putFailure rvar (ex :: SomeException)
      Right a -> putSuccess rvar a

fetchLocalExecutorReq :: FilePath -> LocalExecutorReq a -> IO a
fetchLocalExecutorReq workDir req@(Clone src) = do
  let dest = "artifact-" <> show (hash req)
  createDirectoryIfMissing True (workDir </> dest)
  withCurrentDirectory src $
    Git.copyFiles (workDir </> dest)
  return (HostDir dest)
fetchLocalExecutorReq workDir req@(LocalExec cmd args opts) = do
  -- parse run options
  let needs = [(artifact, dst) | Need artifact dst <- opts]
  let image = last $ Repository "alpine" : [x | Image x <- opts]
  let caches = [x | Cache x <- opts]
  let envs = concat [x | Environment x <- opts]
  let secrets = [x | Secret x <- opts]
  -- create cache directory
  cwd <- getCurrentDirectory
  let cacheDir = cwd </> "_build" </> "caches"
  createDirectoryIfMissing True cacheDir
  -- create output directory
  let out = "output-" <> show (hash req)
  createDirectoryIfMissing True (workDir </> out)
  -- Docker run arguments
  let workspace = "/tmp/build" -- the working directory _inside_ the container
  let argOutputVolume = ["--volume", (workDir </> out) <> ":" <> workspace </> "output"]
  let argWkDir = ["--workdir", workspace]
  let argNeedVolumes = concatMap (\(HostDir n, dst) -> ["--volume", (workDir </> n) <> ":" <> workspace </> dst]) needs
  let argCacheVolumes = concatMap (\c -> ["--volume", cacheDir </> show (hash (req, c)) <> ":" <> workspace </> c]) caches
  let argEnv = concatMap (\(name, value) -> ["--env", name <> "=" <> value]) (envs <> [("HOME", workspace)])
  let argSecrets = concatMap (\e -> ["--env", e]) secrets
  let argCmd = cmd : args
  -- Load the provided image, if needed
  case image of
    ImageTarGz _ (HostDir tarGz) -> callProcess "docker" ["load", "--input", workDir </> tarGz]
    _ -> return ()
  let imageName = case image of
        (Repository n) -> n
        (ImageTarGz n _) -> n
  callProcess
    "docker"
    ( ["run", "--rm", "-i"]
        <> argWkDir
        <> argEnv
        <> argSecrets
        <> argNeedVolumes
        <> argCacheVolumes
        <> argOutputVolume
        <> [imageName]
        <> argCmd
    )
  return (HostDir out)
