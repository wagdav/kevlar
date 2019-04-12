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
          , params =
              [] : List types.Param
          }
        : types.Step
    , Image =
        alpine
    }
