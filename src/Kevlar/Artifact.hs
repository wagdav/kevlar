module Kevlar.Artifact
  ( Artifact(..)
  , Last(..)
  )
where

import           Data.Monoid

type DockerImageId = String
type Environment = [(String, String)]
type Secret = (String, String)
type Volume = (String, FilePath)

data Artifact
  = Artifact
  { volumes :: [Volume]
  , dockerImage :: Last (String, DockerImageId)
  , envVars :: Environment
  , artifactSecrets :: [Secret]
  }
 deriving (Show, Eq, Read)

instance Semigroup Artifact where
  x <> y = Artifact
    (volumes x <> volumes y)
    (dockerImage x <> dockerImage y)
    (envVars x <> envVars y)
    (artifactSecrets x <> artifactSecrets y)

instance Monoid Artifact where
  mempty = Artifact [] (Last Nothing) [] []
