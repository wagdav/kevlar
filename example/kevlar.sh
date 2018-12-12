#!/bin/sh
mkdir -p _shake
ghc --make app/Main.hs -rtsopts -threaded -with-rtsopts=-I0 -outputdir=_shake -o _shake/build && _shake/build --color "$@"
