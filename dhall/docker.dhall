let types = ./../dhall/types.dhall

let defaults = ./../dhall/defaults.dhall

let build =
        λ(image : Text)
      → λ(repo : Text)
      →     defaults.Action
          ⫽ { image =
                None Text
            , script =
                ''
                docker build -t ${image} src/docker/${image}
                docker save -o output/image.tar ${image}
                ''
            , need =
                [ types.Need.Fetch { src = repo, name = "src" } ]
            }
        : types.Action

let loadFromStep =
        λ(step : types.Step)
      → λ(action : types.Action)
      → let useImage =
              { load =
                  Some "${step.name}/image.tar"
              , need =
                    action.need
                  # [ types.Need.Output { name = step.name } ]
              }

        in  action ⫽ useImage : types.Action

in  { build = build, loadFromStep = loadFromStep }
