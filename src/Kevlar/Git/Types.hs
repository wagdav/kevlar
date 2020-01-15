{-# LANGUAGE DeriveGeneric #-}

module Kevlar.Git.Types where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

type Revision = String

data Repository
  = WorkingCopy FilePath
  | Url String (Maybe Revision)
  deriving (Eq, Show, Generic)

instance Hashable Repository
