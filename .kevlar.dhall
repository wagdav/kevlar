let ci = ./dhall/ci.dhall

let build =
        λ(repo : Text)
      → ci.usingSourceBuiltImage
        "kevlar-builder"
        (   ci.Step
          ⫽ { name =
                "build"
            , script =
                ./ci/build.sh as Text
            , need =
                [ ci.fetch { src = repo, name = "src" } ]
            , caches =
                [ ".stack" ]
            }
        )
        repo

let publish =
        λ(repo : Text)
      → ci.usingSourceBuiltImage
        "kevlar-publish"
        (   ci.Step
          ⫽ { name =
                "publish"
            , script =
                ./ci/publish.sh as Text
            , need =
                [ ci.output "build" ]
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
        )
        repo

in  λ(repo : Text) → { steps = build repo # publish repo }
