let ci = ./../dhall/ci.dhall

let bakeBuilderImage =
      { name =
          "bakeBuilderImage"
      , action =
          ci.docker.build "hello-world-builder"
      }

let build =
      { name =
          "build"
      , action =
            λ(repo : Text)
          → ci.docker.loadFromStep
            bakeBuilderImage
            (   ci.Action
              ⫽ { script =
                    ./build.sh as Text
                , need =
                    [ ci.fetch repo "src" ]
                , image =
                    Some "hello-world-builder"
                }
            )
      }

let bakeTesterImage =
      { name =
          "bakeTesterImage"
      , action =
          ci.docker.build "hello-world-tester"
      }

let test =
      { name =
          "test"
      , action =
            λ ( repo
              : Text
              )
          → ci.docker.loadFromStep
            bakeTesterImage
            (   ci.Action
              ⫽ { script =
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
                , image =
                    Some "hello-world-tester"
                , need =
                    [ ci.output build ]
                , environment =
                    [ { name = "HELLO", value = "world!" } ]
                }
            )
      }

in  { steps = [ build, test, bakeBuilderImage, bakeTesterImage ] }
