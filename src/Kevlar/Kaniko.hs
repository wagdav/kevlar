module Kevlar.Kaniko (build) where

import Kevlar
import System.FilePath

-- | Build a Docker image with Kaniko
-- See: https://github.com/GoogleContainerTools/kaniko
build :: String -> FilePath -> [RunOption] -> Task DockerImage
build destination context opts = do
  let argDockerfile = ["--dockerfile", "Dockerfile"]
  let argDestination = ["--destination", destination]
  let argTarPath = ["--tarPath", "output/image.tar.gz"]
  let argContext = ["--context", context]
  let argNoPush = ["--no-push"]
  HostDir out <-
    run
      (argDockerfile <> argDestination <> argTarPath <> argContext <> argNoPush)
      ([Image (Repository "gcr.io/kaniko-project/executor:latest")] <> opts)
  return $ ImageTarGz destination (out </> "image.tar.gz")
