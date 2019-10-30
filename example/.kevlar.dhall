let Kevlar = ./../dhall/package.dhall

let bakeBuilderImage =
      Kevlar.Step::{
      , name = "bakeBuilderImage"
      , action = Kevlar.docker.build "hello-world-builder"
      }

let build =
      Kevlar.Step::{
      , name = "build"
      , action =
            λ(repo : Text)
          → Kevlar.docker.loadFromStep
              bakeBuilderImage
              Kevlar.Action::{
              , script = ./build.sh as Text
              , need = [ Kevlar.fetch repo "src" ]
              , image = Some "hello-world-builder"
              }
      }

let bakeTesterImage =
      Kevlar.Step::{
      , name = "bakeTesterImage"
      , action = Kevlar.docker.build "hello-world-tester"
      }

let test =
      Kevlar.Step::{
      , name = "test"
      , action =
            λ ( repo
              : Text
              )
          → Kevlar.docker.loadFromStep
              bakeTesterImage
              Kevlar.Action::{
              , script =
                  ''
                  #!/bin/sh
                  set -e
                  echo "Artifacts from previous steps are available in this container"
                  tree -L 1

                  echo
                  echo "Environment variables can be specified in the step: \$HELLO=$HELLO"

                  echo
                  echo "Running the output artifacts from the 'build' step"
                  ${build.name}/hello
                  ''
              , image = Some "hello-world-tester"
              , need = [ Kevlar.output build ]
              , environment = toMap { HELLO = "world!" }
              }
      }

in  { steps = [ build, test, bakeBuilderImage, bakeTesterImage ] }
