let Kevlar = ./dhall/package.dhall

let bakeBuilderImage =
      Kevlar.Step::{
      , name = "bakeBuilderImage"
      , action = Kevlar.docker.build "kevlar-builder"
      }

let build =
      Kevlar.Step::{
      , name = "build"
      , action =
            λ(repo : Text)
          → Kevlar.docker.loadFromStep
              bakeBuilderImage
              Kevlar.Action::{
              , script = ./ci/build.sh as Text
              , image = Some "kevlar-builder"
              , need = [ Kevlar.fetch repo "src" ]
              , caches = [ ".stack" ]
              }
      }

let bakePublishImage =
      Kevlar.Step::{
      , name = "bakePublishImage"
      , action = Kevlar.docker.build "kevlar-publish"
      }

let publish =
      Kevlar.Step::{
      , name = "publish"
      , action =
            λ(repo : Text)
          → Kevlar.docker.loadFromStep
              bakePublishImage
              Kevlar.Action::{
              , script = ./ci/publish.sh as Text
              , image = Some "kevlar-publish"
              , need = [ Kevlar.output build ]
              , environment =
                  toMap
                    { GITHUB_ACCESS_TOKEN = env:GITHUB_ACCESS_TOKEN as Text ? ""
                    , KEVLAR_VERSION = env:KEVLAR_VERSION as Text ? ""
                    }
              }
      }

in  { steps = [ bakeBuilderImage, bakePublishImage, build, publish ] }
