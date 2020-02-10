#!/bin/sh
set -e
OUTPUT=${1:-build}
mkdir -p "$OUTPUT"
gcc -Wall hello.c -o "$OUTPUT"/hello
