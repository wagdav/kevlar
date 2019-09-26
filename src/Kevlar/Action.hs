{-# LANGUAGE DeriveGeneric #-}

module Kevlar.Action where

import Kevlar.EnvVar
import Kevlar.Need

import Dhall


data Action = Action
  { shell :: Text
  , script :: Text
  , image :: Maybe Text
  , load :: Maybe Text
  , need :: Vector Need
  , caches :: Vector Text
  , environment :: Vector EnvVar
  } deriving (Generic, Show)

instance Interpret Action
