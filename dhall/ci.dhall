{- A high-level module to be used in CI pipeline definitions -}
let types = ./../dhall/types.dhall

let defaults = ./../dhall/defaults.dhall

let output = λ(name : Text) → types.Need.Output { name = name }

in  { Step =
        defaults.Step
    , bakeDockerImage =
        ./../dhall/bakeDockerImage.dhall
    , loadFromStep =
        ./../dhall/loadFromStep.dhall
    , fetch =
        types.Need.Fetch
    , output =
        output
    }
