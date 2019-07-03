{- Kevlar standard library -}
let Need = < Fetch : { src : Text, name : Text } | Output : { name : Text } >

let EnvVar = { name : Text, value : Text }

let Action =
      { shell :
          Text
      , script :
          Text
      , image :
          Optional Text
      , load :
          Optional Text
      , need :
          List Need
      , caches :
          List Text
      , environment :
          List EnvVar
      }

let Step = { name : Text, action : Text → Action }

let Config = { steps : List Step }

in  { Action =
        Action
    , Config =
        Config
    , Need =
        Need
    , Step =
        Step
    , EnvVar =
        EnvVar
    }
