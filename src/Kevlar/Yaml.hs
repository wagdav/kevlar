{-# LANGUAGE OverloadedStrings #-}
module Kevlar.Yaml where

import           Data.Aeson.Types
import           Data.Char                     as Char
import           Data.Yaml                     as Y

import           Kevlar.Pipeline

-- FromJSON instances
instance FromJSON Pipeline where
  parseJSON = genericParseJSON options

instance FromJSON Step where
  parseJSON = genericParseJSON options { fieldLabelModifier = strip "_step" }

instance FromJSON StepAction where
  parseJSON = genericParseJSON options { fieldLabelModifier = strip "_stepAction" }

instance FromJSON Artifact where
  parseJSON = genericParseJSON options { fieldLabelModifier = strip "_artifact" }

-- ToJSON instances
instance ToJSON Pipeline where
  toJSON = genericToJSON options

instance ToJSON Step where
  toJSON = genericToJSON options { fieldLabelModifier = strip "_step" }

instance ToJSON StepAction where
  toJSON = genericToJSON options { fieldLabelModifier = strip "_stepAction" }

instance ToJSON Artifact where
  toJSON = genericToJSON options { fieldLabelModifier = strip "_artifact" }

options :: Options
options = defaultOptions { sumEncoding = TaggedObject "type" "contents"
                         , constructorTagModifier = fmap Char.toLower
                         , omitNothingFields = True
                         }

strip :: String -> String -> String
strip s = fmap Char.toLower . drop (length s)

readPipeline :: FilePath -> IO Pipeline
readPipeline = decodeFileThrow
