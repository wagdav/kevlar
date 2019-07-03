{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Data.Text as T
import Development.Shake
import System.IO.Temp

import Kevlar
import Kevlar.Config
import Kevlar.Git
import Kevlar.Step

kevlarConfig :: FilePath
kevlarConfig = ".kevlar.dhall"

main :: IO ()
main = withSystemTempDirectory "kevlar" shakeMain

shakeMain :: FilePath -> IO ()
shakeMain src =
  shakeArgs shakeOptions {shakeFiles = "_build"} $ do
    phony "clean" $ do
      putNormal "Cleaning artifacts in _build"
      removeFilesAfter "_build/artifacts" ["//*"]
    phony "purge" $ do
      putNormal "Removing _build"
      removeFilesAfter "_build" ["//*"]
    config <- liftIO $ readConfig kevlarConfig
    forM_ (steps config) (mkRules (T.pack src))
    liftIO $ copyGitFiles src
