let ci = ./../dhall/ci.dhall

let build =
        λ(repo : Text)
      → ci.usingSourceBuiltImage
        "hello-world-builder"
        (   ci.Step
          ⫽ { name =
                "build"
            , script =
                ./build.sh as Text
            , need =
                [ ci.fetch { src = repo, name = "src" } ]
            }
        )
        repo

let test =
      ci.usingSourceBuiltImage
      "hello-world-tester"
      (   ci.Step
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
          , need =
              [ ci.output "build" ]
          , params =
              [ { name = "HELLO", value = "world!" } ]
          }
      )

in  λ(repo : Text) → { steps = build repo # test repo }
