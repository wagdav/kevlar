{- A high-level module to be used in CI pipeline definitions -}
let types = ./types.dhall

let defaults = ./defaults.dhall

let fetch =
        λ(src : Text)
      → λ(name : Text)
      → types.Need.Fetch { src = src, name = name }

let output = λ(step : types.Step) → types.Need.Output { name = step.name }

in  { Action = defaults.Action
    , Step = defaults.Step
    , docker = ./docker.dhall
    , fetch = fetch
    , output = output
    }
