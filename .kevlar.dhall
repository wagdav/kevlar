let Kevlar = ./dhall/package.dhall

let docker = ./dhall/docker.dhall

let builderImage = docker.build "kevlar-builder"

let build =
        λ(repo : Text)
      → Kevlar.Action::{
        , script = ./ci/build.sh as Text
        , image = Some "kevlar-builder"
        , need = [ Kevlar.fetch repo "src", Kevlar.output "builderImage" ]
        , load = Some "builderImage/image.tar"
        , caches = [ ".stack" ]
        }

let publishImage = docker.build "kevlar-publish"

let publish =
        λ(repo : Text)
      → Kevlar.Action::{
        , script = ./ci/publish.sh as Text
        , image = Some "kevlar-publish"
        , need = [ Kevlar.output "build", Kevlar.output "publishImage" ]
        , load = Some "publishImage/image.tar"
        , environment =
            toMap
              { GITHUB_ACCESS_TOKEN = env:GITHUB_ACCESS_TOKEN as Text ? ""
              , KEVLAR_VERSION = env:KEVLAR_VERSION as Text ? ""
              }
        }

let steps =
      toMap
        { builderImage = Kevlar.Step::{ action = builderImage }
        , publishImage = Kevlar.Step::{ action = publishImage }
        , build = Kevlar.Step::{ action = build, requires = [ "builderImage" ] }
        , publish =
            Kevlar.Step::{
            , action = publish
            , requires = [ "build", "publishImage" ]
            }
        }

in  { steps = steps }
