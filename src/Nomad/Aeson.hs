{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module Nomad.Aeson where

import           Nomad.Job
import qualified Data.Char                     as Char

import           Data.Aeson.Types
import           GHC.Generics

deriving instance Generic Artifact
deriving instance Generic Job
deriving instance Generic Nomad
deriving instance Generic ReschedulePolicy
deriving instance Generic Task
deriving instance Generic TaskConfig
deriving instance Generic TaskGroup

instance ToJSON Nomad where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = strip "_nomad" }

instance ToJSON Artifact where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = strip "_artifact" }

instance ToJSON Job where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = strip "_job" }

instance ToJSON TaskGroup where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = strip "_taskGroup" }

instance ToJSON Task where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = strip "_task" }

instance ToJSON TaskConfig where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = fieldLabelModifierTaskConfig
  }

fieldLabelModifierTaskConfig "_taskConfigWorkDir" = "work_dir"
fieldLabelModifierTaskConfig x = Char.toLower <$> strip "_taskConfig" x

instance ToJSON ReschedulePolicy where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = strip "reschedulePolicy" }

strip :: String -> String -> String
strip s = drop (length s)
