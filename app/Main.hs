module Main where

import           Kevlar
import           Kevlar.Pipeline
import           Kevlar.Yaml

import           Control.Monad                  ( forM_ )

import           Development.Shake

kevlarConfig :: FilePath
kevlarConfig = ".kevlar/config.yml"

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = "_build" } $ do
  rulesOracle

  phony "clean" $ do
    putNormal "Cleaning artifacts in _build"
    removeFilesAfter "_build/artifacts" ["//*"]

  phony "purge" $ do
    putNormal "Removing _build"
    removeFilesAfter "_build" ["//*"]

  pipeline <- liftIO $ readPipeline kevlarConfig
  forM_ (steps pipeline) $ \step -> do
    mkRules step

    phony (name step) $ need [kevlarConfig, build (name step)]
