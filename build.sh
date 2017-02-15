#!/bin/bash

for version in 12.04 14.04 16.04; do
    docker-compose -f build/docker-compose-ubuntu-$version.yml up
done

docker-compose -f build/docker-compose-static.yml up
