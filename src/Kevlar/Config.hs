{-# LANGUAGE OverloadedStrings #-}

module Kevlar.Config where

import Kevlar.Step

import Dhall


type Config = Steps

readConfig :: FilePath -> IO Config
readConfig = inputFile auto
