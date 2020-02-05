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

import Control.Exception (SomeException, try)
import Haxl.Core
import qualified Kevlar.Git as Git
import qualified Kevlar.GitHub as GitHub
import Kevlar.LocalExecutor
import Kevlar.LocalExecutor.DataSource
import Kevlar.Status
import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

kevlarMain :: (Git.Repository -> Task a) -> IO ()
kevlarMain task = do
  repo <- parse =<< getArgs
  GitHub.status repo Pending
  e <- Control.Exception.try $ kevlar (task repo)
  case e of
    Left ex -> do
      print (ex :: SomeException)
      GitHub.status repo Failure
    Right _ -> GitHub.status repo Success

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
parse [path] = return $ Git.WorkingCopy path
parse (url : ref : xs) = mkRepository url ref <$> doesDirectoryExist url

mkRepository :: String -> Git.Revision -> Bool -> Git.Repository
mkRepository url revision local =
  if local
    then Git.WorkingCopy url
    else Git.Url url revision
