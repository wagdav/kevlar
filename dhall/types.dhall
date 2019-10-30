{- Kevlar standard library -}
let Need = < Fetch : { src : Text, name : Text } | Output : { name : Text } >

let EnvVar = { mapKey : Text, mapValue : Text }

let Action =
      { shell : Text
      , script : Text
      , image : Optional Text
      , load : Optional Text
      , need : List Need
      , caches : List Text
      , environment : List EnvVar
      }

let Step = { action : Text → Action, requires : List Text }

in  { Action = Action
    , Need = Need
    , Step = Step
    , EnvVar = EnvVar
    }
