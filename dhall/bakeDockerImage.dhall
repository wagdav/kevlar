let types = ./../dhall/types.dhall

let defaults = ./../dhall/defaults.dhall

in    λ(image : Text)
    → λ(dockerfile : Text)
    →     defaults.Step
        ⫽ { name =
              "${image}"
          , image =
              None types.Image
          , script =
              ''
              cat << 'DOCKERFILE' | docker build -t ${image} -
              ${dockerfile}
              DOCKERFILE

              docker save -o output/image.tar ${image}
              ''
          }
      : types.Step
