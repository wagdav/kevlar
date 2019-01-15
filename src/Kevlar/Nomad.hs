{-# LANGUAGE DeriveGeneric #-}
module Kevlar.Nomad
  ( mkJob
  , Job
  , writeJob
  )
where

import           Data.Aeson                     ( encode )
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy          as B
import qualified Data.Char                     as Char
import           GHC.Generics

data Nomad
  = Nomad
  { nomadJob :: Job
  }
 deriving (Show, Eq, Generic)

data Job
  = Job
  { jobID :: String
  , jobName :: String
  , jobType :: String
  , jobDatacenters :: [String]
  , jobTaskGroups :: [TaskGroup]
  , jobReschedulePolicy :: ReschedulePolicy
  }
 deriving (Show, Eq, Generic)

data TaskGroup
  = TaskGroup
  { taskGroupName :: String
  , taskGroupCount :: Int
  , taskGroupTasks :: [Task]
  }
 deriving (Show, Eq, Generic)

data ReschedulePolicy
  = ReschedulePolicy
  { reschedulePolicyAttempts :: Int
  }
 deriving (Show, Eq, Generic)

data Task
  = Task
  { taskName :: String
  , taskDriver :: String
  , taskConfig :: TaskConfig
  , taskArtifacts :: [Artifact]
  }
 deriving (Show, Eq, Generic)

data TaskConfig
  = TaskConfig
  { taskConfigImage :: String
  , taskConfigCommand :: String
  , taskConfigVolumes :: [String]
  , taskConfigWork_dir :: FilePath
  }
 deriving (Show, Eq, Generic)

data Artifact
  = Artifact
  { artifactGetterSource :: String
  , artifactRelativeDest :: String
  }
 deriving (Show, Eq, Generic)

instance ToJSON Nomad where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = strip "nomad" }

instance ToJSON Artifact where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = strip "artifact" }

instance ToJSON Job where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = strip "job" }

instance ToJSON TaskGroup where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = strip "taskGroup" }

instance ToJSON Task where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = strip "task" }

instance ToJSON TaskConfig where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = fmap Char.toLower . strip "taskConfig"
  }

instance ToJSON ReschedulePolicy where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = strip "reschedulePolicy" }

strip :: String -> String -> String
strip s = drop (length s)

mkJob name image command volumes workDir sources = Job name  -- id
                                               name  -- name
                                               "batch"
                                               ["dc1"]
                                               [TaskGroup name 1 [task]]
                                               (ReschedulePolicy 0)
  where task = Task name "docker" (TaskConfig image command volumes workDir) artifacts
        artifacts = [Artifact s d | (s, d) <- sources]

writeJob :: FilePath -> Job -> IO ()
writeJob p j = B.writeFile p (encode $ Nomad j)
