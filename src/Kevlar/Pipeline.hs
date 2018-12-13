{-# LANGUAGE DeriveGeneric #-}
module Kevlar.Pipeline where

import           Data.Maybe
import           GHC.Generics
import qualified Data.Map.Strict               as Map

type Name = String
type Platform = String

data Pipeline
  = Pipeline
    { steps :: [Step]
    }
  deriving (Eq, Show, Generic)

data Step
  = Step
    { _stepName   :: Name
    , _stepAction :: StepAction
    }
  deriving (Eq, Show, Generic)

data StepAction
  = Script
    { _stepActionPath         :: FilePath
    , _stepActionPlatform     :: Platform
    , _stepActionArtifacts    :: [Artifact]
    , _stepActionEnvironment  :: Maybe (Map.Map String String)
    , _stepActionCaches       :: Maybe [FilePath]
    }
  | Image
    { _stepActionContext       :: FilePath
    }
  deriving (Eq, Show, Generic)


data Artifact
  = Artifact
  { _artifactSource         :: String
  , _artifactDestination    :: Maybe String
  }
  deriving (Eq, Show, Generic)


artifactSource :: Artifact -> String
artifactSource = _artifactSource

artifactDestination :: Artifact -> String
artifactDestination a = fromMaybe (artifactSource a) (_artifactDestination a)

thisRepo :: Artifact -> Bool
thisRepo (Artifact "." _) = True
thisRepo _                = False
