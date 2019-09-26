{-# LANGUAGE DeriveGeneric #-}

module Kevlar.EnvVar where

import Dhall

data EnvVar = EnvVar
  { mapKey :: Text
  , mapValue :: Text
  } deriving (Generic, Show)

instance Interpret EnvVar
