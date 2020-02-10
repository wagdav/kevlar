#!/bin/sh
set -e

OUTPUT=${1:-build}
echo "Artifacts from previous steps are available in this container"
tree -L 1

if [ -z "$HELLO" ]; then
    echo "\$HELLO environment variable is not set!"
    exit 1
fi

echo
echo "Environment variables can be specified in the step: \$HELLO=$HELLO",

echo
echo "Running the output artifacts from the 'build' step",
"$OUTPUT"/hello
