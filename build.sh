#!/usr/bin/env bash

set -eux

docker build . -t dlozeve/sncf
CONTAINER_ID=$(docker run -d dlozeve/sncf)
docker cp "$CONTAINER_ID":/src/sncf .
docker rm -f "$CONTAINER_ID"
