#!/usr/bin/env bash

set -eux

docker build . -f Dockerfile-build -t dlozeve/sncf-bot
CONTAINER_ID=$(docker run -d dlozeve/sncf-bot)
docker cp "$CONTAINER_ID":/src/sncf-static sncf-static
docker rm -f "$CONTAINER_ID"
