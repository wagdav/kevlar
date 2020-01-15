module Kevlar
  ( -- construct tasks
    RunOption (..),
    Artifact (..),
    DockerImage (..),
    Task,
    clone,
    run,
    shell,
    -- run Tasks
    kevlar,
    kevlarMain,
  )
where

import Control.Monad (void)
import Haxl.Core
import Haxl.Prelude
import qualified Kevlar.Git as Git
import Kevlar.LocalExecutor
import Kevlar.LocalExecutor.DataSource
import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

kevlarMain :: (Git.Repository -> Task a) -> IO ()
kevlarMain task = do
  repo <- parse =<< getArgs
  void $ kevlar (task repo)

-- Run a kevlar task
kevlar :: Task a -> IO a
kevlar task = do
  cwd <- getCurrentDirectory
  let cacheDir = cwd </> "_build" </> "caches"
  withSystemTempDirectory "kevlar" $ \workDir -> do
    state <- initGlobalState 10 (WorkDir workDir) (CacheDir cacheDir)
    env <- initEnv (stateSet state stateEmpty) ()
    runHaxl env task

parse :: [String] -> IO Git.Repository
parse [] = return $ Git.WorkingCopy "."
parse [url] = do
  local <- doesDirectoryExist url
  return $
    if local
      then Git.WorkingCopy url
      else Git.Url url Nothing
parse (url : ref : xs) = do
  local <- doesDirectoryExist url
  return $
    if local
      then Git.WorkingCopy url
      else Git.Url url (Just ref)
