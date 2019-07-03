{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Kevlar.Step where

import Kevlar.Action

import Dhall

data Step = Step
  { name :: Text
  , action :: Text -> Action
  } deriving (Generic)

instance Interpret Step

data Steps = Steps
  { steps :: Vector Step
  } deriving (Generic)

instance Interpret Steps
