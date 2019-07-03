let types = ./types.dhall

let action =
        { shell =
            "/bin/bash"
        , script =
            "echo 'hello, kevlar'"
        , image =
            Some "alpine"
        , load =
            None Text
        , need =
            [] : List types.Need
        , caches =
            [] : List Text
        , environment =
            [] : List types.EnvVar
        }
      : types.Action

in  { Action =
        action
    , Config =
        { steps = [] : List types.Step }
    , Step =
        { name = "hello", action = λ(src : Text) → action } : types.Step
    }
