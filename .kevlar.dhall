let ci = ./dhall/ci.dhall

let bakeBuilderImage =
      { name = "bakeBuilderImage", action = ci.docker.build "kevlar-builder" }

let build =
      { name =
          "build"
      , action =
            λ(repo : Text)
          → ci.docker.loadFromStep
            bakeBuilderImage
            (   ci.Action
              ⫽ { script =
                    ./ci/build.sh as Text
                , image =
                    Some "kevlar-builder"
                , need =
                    [ ci.fetch repo "src" ]
                , caches =
                    [ ".stack" ]
                }
            )
      }

let bakePublishImage =
      { name = "bakePublishImage", action = ci.docker.build "kevlar-publish" }

let publish =
      { name =
          "publish"
      , action =
            λ(repo : Text)
          → ci.docker.loadFromStep
            bakePublishImage
            (   ci.Action
              ⫽ { script =
                    ./ci/publish.sh as Text
                , image =
                    Some "kevlar-publish"
                , need =
                    [ ci.output build ]
                , environment =
                    [ { name =
                          "GITHUB_ACCESS_TOKEN"
                      , value =
                          env:GITHUB_ACCESS_TOKEN as Text ? ""
                      }
                    , { name =
                          "KEVLAR_VERSION"
                      , value =
                          env:KEVLAR_VERSION as Text ? ""
                      }
                    ]
                }
            )
      }

in  { steps = [ bakeBuilderImage, bakePublishImage, build, publish ] }
