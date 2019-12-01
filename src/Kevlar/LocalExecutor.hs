{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Kevlar.LocalExecutor where

import Control.Monad
import Data.List (uncons)
import Haxl.Core
import Kevlar.LocalExecutor.DataSource

type Task a = GenHaxl () () a

clone :: String -> Task Artifact
clone src = dataFetch (Clone src)

run :: [String] -> [RunOption] -> Task Artifact
run args opts = case uncons args of
  Just (x, xs) -> dataFetch (LocalExec x xs opts)
  Nothing -> dataFetch (LocalExec "" [] opts)

-- Run the provided lines of text in the shell
shell :: [String] -> [RunOption] -> Task Artifact
shell script = run ["/bin/sh", "-c", unlines script] -- FIXME pipefail option
