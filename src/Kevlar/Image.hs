{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Kevlar.Image where

import Dhall

data Image = Image
  { name :: Text
  , load :: Maybe Text
  } deriving (Generic, Show)

instance Interpret Image
