{-# LANGUAGE TemplateHaskell #-}

module Nomad.Job where

import           Control.Lens

data Nomad
  = Nomad
  { _nomadJob :: Job
  }
 deriving (Show, Eq)

data Job
  = Job
  { _jobID :: String
  , _jobName :: String
  , _jobType :: String
  , _jobDatacenters :: [String]
  , _jobTaskGroups :: [TaskGroup]
  , _jobReschedulePolicy :: ReschedulePolicy
  }
 deriving (Show, Eq)

data TaskGroup
  = TaskGroup
  { _taskGroupName :: String
  , _taskGroupCount :: Int
  , _taskGroupTasks :: [Task]
  }
 deriving (Show, Eq)

data ReschedulePolicy
  = ReschedulePolicy
  { _reschedulePolicyAttempts :: Int
  }
 deriving (Show, Eq)

data Task
  = Task
  { _taskName :: String
  , _taskDriver :: String
  , _taskConfig :: TaskConfig
  , _taskArtifacts :: [Artifact]
  }
 deriving (Show, Eq)

data TaskConfig
  = TaskConfig
  { _taskConfigImage :: String
  , _taskConfigCommand :: String
  , _taskConfigVolumes :: [String]
  , _taskConfigWorkDir :: FilePath  -- FIXME JSON is work_dir !!
  }
 deriving (Show, Eq)

data Artifact
  = Artifact
  { _artifactGetterSource :: String
  , _artifactRelativeDest :: String
  }
 deriving (Show, Eq)

makeLenses ''Artifact
makeLenses ''Job
makeLenses ''Nomad
makeLenses ''Task
makeLenses ''TaskConfig
makeLenses ''TaskGroup
