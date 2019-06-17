{-# LANGUAGE DeriveGeneric #-}

module Kevlar.EnvVar where

import Dhall

data EnvVar = EnvVar
  { name :: Text
  , value :: Text
  } deriving (Generic, Show)

instance Interpret EnvVar
