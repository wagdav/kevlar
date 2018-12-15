module Kevlar.Artifact
  ( Artifact(..)
  , Last(..)
  )
where

import           Data.Monoid

type DockerImage = String
type Environment = [(String, String)]
type Volume = (String, FilePath)

data Artifact
  = Artifact
  { volumes :: [Volume]
  , dockerImage :: Last DockerImage
  , envVars :: Environment
  }
 deriving (Show, Eq, Read)

instance Semigroup Artifact where
  x <> y = Artifact
    (volumes x <> volumes y)
    (dockerImage x <> dockerImage y)
    (envVars x <> envVars y)

instance Monoid Artifact where
  mempty = Artifact [] (Last Nothing) []
