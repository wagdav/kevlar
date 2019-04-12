{-# LANGUAGE OverloadedStrings #-}

module Kevlar.Config where

import Kevlar.Image
import Kevlar.Step

import Dhall


type Config = Text -> Steps

readConfig :: FilePath -> IO Config
readConfig = inputFile auto
