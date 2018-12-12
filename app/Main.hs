module Main where

import           Kevlar
import           Kevlar.Pipeline
import           Kevlar.Yaml

import Control.Monad (mapM_)

import           Development.Shake

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = "_build" } $ do
  rulesOracle

  phony "clean" $ do
    putNormal "Cleaning files in _build"
    removeFilesAfter "_build" ["//*"]

  pipeline <- liftIO $ readPipeline ".kevlar/config.yml"
  mapM_ mkRules (steps pipeline)
