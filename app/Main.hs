module Main where

import           Kevlar
import           Kevlar.Pipeline
import           Kevlar.Yaml

import           Control.Monad                  ( forM_ )

import           Development.Shake

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = "_build" } $ do
  rulesOracle

  phony "clean" $ do
    putNormal "Cleaning artifacts in _build"
    removeFilesAfter "_build/artifacts" ["//*"]

  phony "purge" $ do
    putNormal "Removing _build"
    removeFilesAfter "_build" ["//*"]

  pipeline <- liftIO $ readPipeline ".kevlar/config.yml"
  forM_ (steps pipeline) $ \step -> do
    mkRules step

    phony (name step) $ do
      version <- gitHash
      need [done version (name step)]
