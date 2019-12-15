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
    kevlar_,
    kevlarMain,
  )
where

import Control.Monad (void)
import Haxl.Core
import Haxl.Prelude
import Kevlar.LocalExecutor
import Kevlar.LocalExecutor.DataSource
import System.IO.Temp (withSystemTempDirectory)

kevlarMain = void . kevlar

-- Run a kevlar task
kevlar :: Task a -> IO a
kevlar task = withSystemTempDirectory "kevlar" $ \workDir -> do
  state <- initGlobalState 10 workDir
  env <- initEnv (stateSet state stateEmpty) ()
  runHaxl env task

kevlar_ = void . kevlar
