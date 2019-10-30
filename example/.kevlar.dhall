let Kevlar = ./../dhall/package.dhall

let docker = ../dhall/docker.dhall

let bakeBuilderImage = docker.build "hello-world-builder"

let build =
        λ(repo : Text)
      → Kevlar.Action::{
        , script = ./build.sh as Text
        , need = [ Kevlar.fetch repo "src", Kevlar.output "bakeBuilderImage" ]
        , image = Some "hello-world-builder"
        , load = Some "bakeBuilderImage/image.tar"
        }

let bakeTesterImage = docker.build "hello-world-tester"

let test =
        λ ( repo
          : Text
          )
      → Kevlar.Action::{
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
            build/hello
            ''
        , image = Some "hello-world-tester"
        , need = [ Kevlar.output "build", Kevlar.output "bakeTesterImage" ]
        , load = Some "bakeTesterImage/image.tar"
        , environment = toMap { HELLO = "world!" }
        }

let steps =
      toMap
        { build =
            Kevlar.Step::{ action = build, requires = [ "bakeBuilderImage" ] }
        , bakeBuilderImage = Kevlar.Step::{ action = bakeBuilderImage }
        , bakeTesterImage = Kevlar.Step::{ action = bakeTesterImage }
        , test =
            Kevlar.Step::{
            , action = test
            , requires = [ "build", "bakeTesterImage" ]
            }
        }

in  { steps = steps }
