#!/bin/sh
set -e
stack install \
  --allow-different-user \
  --test \
  --local-bin-path "$KEVLAR_OUTPUT" \
  --flag=kevlar:static
