{-# LANGUAGE DeriveGeneric #-}
module Kevlar.Nomad where

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
  }
 deriving (Show, Eq, Generic)

data TaskGroup
  = TaskGroup
  { taskGroupName :: String
  , taskGroupCount :: Int
  , taskGroupTasks :: [Task]
  }
 deriving (Show, Eq, Generic)

data Task
  = Task
  { taskName :: String
  , taskDriver :: String
  , taskConfig :: TaskConfig
  }
 deriving (Show, Eq, Generic)

data TaskConfig
  = TaskConfig
  { taskConfigImage :: String
  , taskConfigCommand :: String
  }
 deriving (Show, Eq, Generic)

instance ToJSON Nomad where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = strip "nomad" }

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

strip :: String -> String -> String
strip s = drop (length s)

simpleTask :: Job
simpleTask = Job "example"
                 "example"
                 "batch"
                 ["dc1"]
                 [TaskGroup "example" 1 [task]]
 where
  task = Task "redis" "docker" (TaskConfig "hello-world-builder:e25773f" "ls")

writeJob :: FilePath -> Job -> IO ()
writeJob p j = B.writeFile p (encode $ Nomad j)
