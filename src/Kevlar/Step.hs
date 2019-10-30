{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Kevlar.Step where

import Kevlar.Action

import Dhall

data Step = Step
  { name :: Text
  , action :: Text -> Action
  } deriving (Generic)

instance FromDhall Step

data Steps = Steps
  { steps :: Vector Step
  } deriving (Generic)

instance FromDhall Steps
