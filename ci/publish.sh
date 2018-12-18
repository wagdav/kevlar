#!/bin/sh
set -e

user=wagdav:"$GITHUB_ACCESS_TOKEN"
draft=true

release=kevlar-$KEVLAR_VERSION-linux-amd64

mkdir "$release"
cp build/kevlar "$release"

asset="$release.tar.gz"
tar cvzf "$asset" "$release"

upload_url=$(curl \
   --silent \
   --user "$user" \
   --request POST \
   --header "Content-Type: application/json" \
   --data "{\"tag_name\":\"$KEVLAR_VERSION\", \
            \"name\":\"$KEVLAR_VERSION\", \
            \"draft\":$draft}" \
   "https://api.github.com/repos/wagdav/kevlar/releases" \
   | jq -r '.upload_url[:-13]')

curl \
   --silent \
   --user "$user" \
   --request POST \
   --header "Content-Type: application/gzip" \
   --data-binary @"$asset" \
   "$upload_url?name=$asset" > /dev/null

echo "Release $KEVLAR_VERSION is published."
