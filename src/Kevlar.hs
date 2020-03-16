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
    kevlarMainWith,
  )
where

import Control.Exception (SomeException, try)
import Haxl.Core
import qualified Kevlar.Git as Git
import qualified Kevlar.GitHub as GitHub
import Kevlar.LocalExecutor
import Kevlar.LocalExecutor.DataSource
import Kevlar.Status
import System.Directory (doesDirectoryExist, getCurrentDirectory, withCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

kevlarMain :: (Git.Repository -> Task a) -> IO ()
kevlarMain task = kevlarMainWith task (\_ _ -> return ())

kevlarMainWith :: (Git.Repository -> Task a) -> (Git.Repository -> a -> IO ()) -> IO ()
kevlarMainWith task action = do
  repo <- parse =<< getArgs
  GitHub.status repo Pending
  e <- Control.Exception.try
    $ withSystemTempDirectory "kevlar"
    $ \workDir -> do
      out <- kevlar workDir (task repo)
      withCurrentDirectory workDir $ action repo out
  case e of
    Left ex -> do
      print (ex :: SomeException)
      GitHub.status repo Failure
    Right _ -> GitHub.status repo Success

-- Run a kevlar task
kevlar :: FilePath -> Task a -> IO a
kevlar workDir task = do
  cwd <- getCurrentDirectory
  let cacheDir = cwd </> "_build" </> "caches"
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
