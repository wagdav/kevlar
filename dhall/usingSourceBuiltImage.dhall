let types = ./../dhall/types.dhall

let defaults = ./../dhall/defaults.dhall

let bakeStepName = λ(imageName : Text) → "bake-${imageName}-image"

let bakeImage =
        λ(image : Text)
      → λ(repo : Text)
      →     defaults.Step
          ⫽ { name =
                bakeStepName image
            , image =
                None types.Image
            , script =
                ''
                docker build -t ${image} src/docker/${image}
                docker save -o output/image.tar ${image}
                ''
            , need =
                [ types.Need.Fetch { src = repo, name = "src" } ]
            }
        : types.Step

in    λ(imageName : Text)
    → λ(step : types.Step)
    → λ(repo : Text)
    → [ bakeImage imageName repo
      ,   step
        ⫽ { image =
              Some
              (   defaults.Image
                ⫽ { name =
                      imageName
                  , load =
                      Some "${bakeStepName imageName}/image.tar"
                  }
              )
          , need =
                step.need
              # [ types.Need.Output { name = bakeStepName imageName } ]
          }
      ] : List types.Step
