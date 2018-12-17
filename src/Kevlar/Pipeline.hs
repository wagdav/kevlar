module Kevlar.Pipeline where

import           Data.Maybe
import qualified Data.Map.Strict               as Map

type Name = String
type Platform = String
type Url = String

data Pipeline
  = Pipeline
    { steps :: [Step]
    }
  deriving (Eq, Show)

data Step
  = Script
    { name      :: Name
    , script    :: FilePath
    , caches    :: [FilePath]
    , _need     :: [Name]
    }
  | Environment
    { name        :: Name
    , environment :: Map.Map String String
    }
  | DockerImage
    { name    :: Name
    , context :: FilePath
    , _need   :: [Name]
    }
  | Source
    { name   :: Name
    , source :: Url
    }
  deriving (Eq, Show)
