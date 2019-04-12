let ci = ./dhall/ci.dhall

let bakeBuilderImage =
      ci.bakeDockerImage
      "kevlar-builder"
      ./docker/kevlar-builder/Dockerfile as Text

let bakePublishImage =
      ci.bakeDockerImage
      "kevlar-publish"
      ./docker/kevlar-publish/Dockerfile as Text

let build =
        λ(src : Text)
      →   ci.Step
        ⫽ { name =
              "build"
          , script =
              ./ci/build.sh as Text
          , image =
              ci.loadFromStep bakeBuilderImage
          , need =
              [ ci.fetch { src = src, name = "src" }
              , ci.output "${bakeBuilderImage.name}"
              ]
          , caches =
              [ ".stack" ]
          }

let publish =
        ci.Step
      ⫽ { name =
            "publish"
        , script =
            ./ci/publish.sh as Text
        , image =
            ci.loadFromStep bakePublishImage
        , need =
            [ ci.output "${bakePublishImage.name}", ci.output "build" ]
        , caches =
            [ ".stack" ]
        , params =
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

in    λ(repo : Text)
    → { steps = [ bakeBuilderImage, build repo, bakePublishImage, publish ] }
