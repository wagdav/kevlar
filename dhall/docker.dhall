let types = ./../dhall/types.dhall

let defaults = ./../dhall/defaults.dhall

let build =
        λ(image : Text)
      → λ(ctx : types.Context)
      →     defaults.Action
          ⫽ { image = None Text
            , script =
                ''
                docker build -t ${image} src/docker/${image}
                docker save -o output/image.tar ${image}
                ''
            , need = [ types.Need.Fetch { src = ctx.repo, name = "src" } ]
            }
        : types.Action

in  { build = build }
