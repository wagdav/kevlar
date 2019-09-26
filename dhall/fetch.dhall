let types = ./types.dhall

in  λ(src : Text) → λ(dst : Text) → { _1 = src, _2 = dst } : types.Input
