{-# LANGUAGE ApplicativeDo #-}

import Kevlar
import Kevlar.Kaniko as Kaniko

-- Helper function to build a Docker image
dockerBuild src name = Kaniko.build name ("docker/" <> name) [Need src ""]

-- | Example of a build-and-run pipeline
--
-- This pipeline models a typical scenario where an application is built in a
-- Docker image containing the build toolchain.  Then the build artifact is
-- transferred into a deployment image.  This way the build toolchain is not
-- part of the released application image.
--
-- The pipeline builds the Docker images based on the Dockerfiles in the
-- repository itself.  The build and run scripts are run inside the
-- pipeline-built containers.
buildAndRun repo = do
  src <- clone repo
  -- Docker images
  builderImage <- dockerBuild src "hello-world-builder"
  testerImage <- dockerBuild src "hello-world-tester"
  -- Compile the source code
  binary <-
    shell
      ["gcc -Wall src/hello.c -o output/hello"]
      [Need src "src", Image builderImage]
  -- Run the compiled binary
  shell
    [ "#!/bin/sh",
      "set -e",
      "echo \"Artifacts from previous steps are available in this container\"",
      "tree -L 1",
      "",
      "echo",
      "echo \"Environment variables can be specified in the step: \\$HELLO=$HELLO\"",
      "",
      "echo",
      "echo \"Running the output artifacts from the 'build' step\"",
      "build/hello"
    ]
    [ Need binary "build",
      Image testerImage,
      Environment [("HELLO", "world!")]
    ]

main :: IO ()
main = kevlarMain buildAndRun
