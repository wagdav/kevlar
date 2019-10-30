let types = ./types.dhall

let defaults = ./defaults.dhall

let fetch =
        λ(src : Text)
      → λ(name : Text)
      → types.Need.Fetch { src = src, name = name }

let output = λ(step : types.Step) → types.Need.Output { name = step.name }

in  { Action = { Type = types.Action, default = defaults.Action }
    , Step = { Type = types.Step, default = defaults.Step }
    , fetch = fetch
    , output = output
    , docker = ./docker.dhall
    }
