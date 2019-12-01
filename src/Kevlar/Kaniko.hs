module Kevlar.Kaniko (build) where

import Kevlar
import System.FilePath

-- | Build a Docker image with Kaniko
-- See: https://github.com/GoogleContainerTools/kaniko
build :: String -> FilePath -> [RunOption] -> Task Artifact
build destination context opts = do
  let argDockerfile = ["--dockerfile", "Dockerfile"]
  let argDestination = ["--destination", destination]
  let argTarPath = ["--tarPath", "output/image.tar.gz"]
  let argContext = ["--context", context]
  let argNoPush = ["--no-push"]
  HostDir out <-
    run
      (argDockerfile <> argDestination <> argTarPath <> argContext <> argNoPush)
      ([Image "gcr.io/kaniko-project/executor:latest"] <> opts)
  return $ HostDir $ out </> "image.tar.gz"
