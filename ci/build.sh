#!/bin/sh
set -e
(cd src && stack install \
    --test \
    --local-bin-path "$KEVLAR_OUTPUT" \
    --ghc-options "-optl-static -fPIC -optc-Os")
