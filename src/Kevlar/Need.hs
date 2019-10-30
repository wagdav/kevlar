{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Kevlar.Need where

import Dhall

data Need
  = Fetch { src :: Text
          , name :: Text }
  | Output { name :: Text }
  deriving (Generic, Show)

instance FromDhall Need
