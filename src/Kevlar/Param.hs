{-# LANGUAGE DeriveGeneric #-}

module Kevlar.Param where

import Dhall

data Param = Param
  { name :: Text
  , value :: Text
  } deriving (Generic, Show)

instance Interpret Param
