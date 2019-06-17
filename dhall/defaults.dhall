let types = ./types.dhall

let alpine = { name = "alpine", load = None Text } : types.Image

in  { Config =
        { steps = [] : List types.Step }
    , Step =
          { name =
              "hello"
          , shell =
              "/bin/bash"
          , script =
              "echo 'hello, kevlar'"
          , image =
              Some alpine
          , need =
              [] : List types.Need
          , caches =
              [] : List Text
          , environment =
              [] : List types.EnvVar
          }
        : types.Step
    , Image =
        alpine
    }
