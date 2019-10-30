{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Kevlar.Step where

import Kevlar.Action

import Dhall

data Step = Step
  { mapKey :: Text
  , mapValue :: StepDef
  } deriving (Generic)

instance FromDhall Step

data StepDef = StepDef
  { action :: Text -> Action
  , requires :: Vector Text
  } deriving (Generic)

instance FromDhall StepDef

data Steps = Steps
  { steps :: Vector Step
  } deriving (Generic)

instance FromDhall Steps
