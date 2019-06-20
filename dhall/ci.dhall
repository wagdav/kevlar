{- A high-level module to be used in CI pipeline definitions -}
let types = ./types.dhall

let defaults = ./defaults.dhall

let output = λ(name : Text) → types.Need.Output { name = name }

in  { Step =
        defaults.Step
    , usingSourceBuiltImage =
        ./usingSourceBuiltImage.dhall
    , fetch =
        types.Need.Fetch
    , output =
        output
    }
