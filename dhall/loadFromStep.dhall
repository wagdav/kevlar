let defaults = ./defaults.dhall

let types = ./types.dhall

in    λ(step : types.Step)
    →   Some
        (   defaults.Image
          ⫽ { name =
                "${step.name}"
            , load =
                Some "${step.name}/${step.name}.tar"
            }
        )
      : Optional types.Image
