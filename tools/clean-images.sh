#!/bin/sh
docker rmi $(docker images "$1" --format '{{ .Repository }}:{{ .Tag }}')
