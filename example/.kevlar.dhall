let ci = ./../dhall/ci.dhall

let bakeBuilderImage =
      ci.bakeDockerImage
      "hello-world-builder"
      ''
      FROM alpine
      RUN apk update && apk add build-base
      ''

let bakeTesterImage =
      ci.bakeDockerImage
      "hello-world-tester"
      ''
      FROM alpine
      RUN apk update && apk add tree
      ''

let build =
        λ(src : Text)
      →   ci.Step
        ⫽ { name =
              "build"
          , script =
              ./build.sh as Text
          , image =
              ci.loadFromStep bakeBuilderImage
          , need =
              [ ci.fetch { src = src, name = "src" }
              , ci.output "${bakeBuilderImage.name}"
              ]
          }

let test =
        ci.Step
      ⫽ { name =
            "test"
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
        , image =
            ci.loadFromStep bakeTesterImage
        , need =
            [ ci.output "build", ci.output "${bakeTesterImage.name}" ]
        , params =
            [ { name = "HELLO", value = "world!" } ]
        }

in    λ(repo : Text)
    → { steps = [ build repo, test, bakeBuilderImage, bakeTesterImage ] }
