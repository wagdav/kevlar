#!/bin/sh
set -e
echo "Artifacts from previous steps are available in this container"
tree -L 1

echo
echo "Environment variables can be specified in the step: \$HELLO=$HELLO"

echo
echo "Running the output artifacts from the 'build' step"
build/hello
