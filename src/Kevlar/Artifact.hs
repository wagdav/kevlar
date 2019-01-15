module Kevlar.Artifact
  ( Artifact(..)
  , Last(..)
  , Volume
  )
where

import           Data.Monoid

type DockerImageId = String
type Parameter = (String, String)
type Secret = (String, String)
type Source = (String, String)
type Volume = (String, FilePath)

data Artifact
  = Artifact
  { volumes :: [Volume]
  , dockerImage :: Last (String, DockerImageId)
  , artifactParameters :: [Parameter]
  , artifactSecrets :: [Secret]
  , artifactSources :: [Source]
  }
 deriving (Show, Eq, Read)

instance Semigroup Artifact where
  x <> y = Artifact
    (volumes x <> volumes y)
    (dockerImage x <> dockerImage y)
    (artifactParameters x <> artifactParameters y)
    (artifactSecrets x <> artifactSecrets y)
    (artifactSources x <> artifactSources y)

instance Monoid Artifact where
  mempty = Artifact [] (Last Nothing) [] [] []
