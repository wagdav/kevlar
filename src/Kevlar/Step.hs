{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Kevlar.Step where

import Kevlar.Image
import Kevlar.Need
import Kevlar.Param

import Dhall

data Step = Step
  { name :: Text
  , shell :: Text
  , script :: Text
  , image :: Maybe Image
  , need :: Vector Need
  , caches :: Vector Text
  , params :: Vector Param
  } deriving (Generic, Show)

instance Interpret Step

data Steps = Steps
  { steps :: Vector Step
  } deriving (Generic, Show)

instance Interpret Steps
