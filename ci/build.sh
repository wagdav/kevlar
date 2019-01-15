#!/bin/sh
set -e
(cd src && stack --allow-different-user install \
    --test \
    --local-bin-path "$KEVLAR_OUTPUT" \
    --ghc-options "-optl-static -fPIC -optc-Os")
