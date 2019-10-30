let types = ./types.dhall

let action =
      { shell = "/bin/bash"
      , script = "echo 'hello, kevlar'"
      , image = Some "alpine"
      , load = None Text
      , need = [] : List types.Need
      , caches = [] : List Text
      , environment = [] : List types.EnvVar
      }

in  { Action = action, Step = { requires = [] : List Text } }
