{- Kevlar standard library -}
let Image = { name : Text, load : Optional Text }

let Need = < Fetch : { src : Text, name : Text } | Output : { name : Text } >

let Param = { name : Text, value : Text }

let Step =
      { name :
          Text
      , shell :
          Text
      , script :
          Text
      , image :
          Optional Image
      , need :
          List Need
      , caches :
          List Text
      , params :
          List Param
      }

let Config = { steps : List Step }

in  { Config = Config, Image = Image, Need = Need, Step = Step, Param = Param }