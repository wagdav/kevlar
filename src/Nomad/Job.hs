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
   -- | https://www.nomadproject.io/api/json-jobs.html#type
  , _jobType :: String
  -- | https://www.nomadproject.io/api/json-jobs.html#datacenters
  , _jobDatacenters :: [String]
  -- | https://www.nomadproject.io/api/json-jobs.html#taskgroups
  , _jobTaskGroups :: [TaskGroup]
  -- | https://www.nomadproject.io/api/json-jobs.html#reschedulepolicy
  , _jobReschedulePolicy :: Maybe ReschedulePolicy
  }
 deriving (Show, Eq)

job :: Job
job = Job
  { _jobName             = "example"
  , _jobID               = "example"
  , _jobType             = "service"
  , _jobDatacenters      = ["dc1"]
  , _jobTaskGroups       = []
  , _jobReschedulePolicy = Nothing
  }

data TaskGroup
  = TaskGroup
  -- | https://www.nomadproject.io/api/json-jobs.html#name
  { _taskGroupName :: String
  -- | https://www.nomadproject.io/api/json-jobs.html#count
  , _taskGroupCount :: Int
  -- | https://www.nomadproject.io/api/json-jobs.html#tasks
  , _taskGroupTasks :: [Task]
  }
 deriving (Show, Eq)

taskGroup :: TaskGroup
taskGroup = TaskGroup
  { _taskGroupName  = "group"
  , _taskGroupCount = 1
  , _taskGroupTasks = []
  }

data ReschedulePolicy
  = ReschedulePolicy
  { _reschedulePolicyAttempts :: Int
  }
 deriving (Show, Eq)

data Task
  = Task
  { _taskName :: String
  -- | https://www.nomadproject.io/api/json-jobs.html#driver
  , _taskDriver :: String
  , _taskConfig :: TaskConfig
  , _taskArtifacts :: [Artifact]
  }
 deriving (Show, Eq)

task :: Task
task = Task
  { _taskName = "task"
  , _taskDriver = "docker"
  , _taskConfig = TaskConfig "" "" [] ""
  , _taskArtifacts = []
  }

data TaskConfig
  = TaskConfig
  { _taskConfigImage :: String
  , _taskConfigCommand :: String
  , _taskConfigVolumes :: [String]
  , _taskConfigWorkDir :: FilePath
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
makeLenses ''Task
makeLenses ''TaskConfig
makeLenses ''TaskGroup
