{-# LANGUAGE DeriveGeneric #-}

module Kevlar.Context where

import Dhall

data Context = Context
  { repo :: Text
  } deriving (Generic, Show)

instance FromDhall Context

instance ToDhall Context

