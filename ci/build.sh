#!/bin/sh
set -ex
(cd src && stack install --local-bin-path "$KEVLAR_OUTPUT")
