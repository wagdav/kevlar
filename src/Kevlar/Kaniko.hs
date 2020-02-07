module Kevlar.Kaniko (build) where

import Kevlar
import System.FilePath

repository name = "localhost:5000/" <> name

-- | Build a Docker image with Kaniko
-- See: https://github.com/GoogleContainerTools/kaniko
build :: String -> FilePath -> [RunOption] -> Task DockerImage
build destination context opts = do
  HostDir out <-
    run
      ( []
          <> ["--dockerfile", "Dockerfile"]
          <> ["--destination", repository destination]
          <> ["--cache=true"]
          <> ["--context", context]
      )
      ([Image (Repository "gcr.io/kaniko-project/executor:v0.16.0")] <> opts)
  return $ Repository (repository destination)
