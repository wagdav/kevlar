#!/bin/sh
set -ex
(cd src && stack --allow-different-user install)
(cd src && stack --allow-different-user exec -- cp "$(which kevlar)" ../output/)
