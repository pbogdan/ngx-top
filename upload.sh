#!/usr/bin/env bash

if [[ -z "${GITHUB_TOKEN}" ]]; then
    echo "Please set GITHUB_TOKEN before uploading."
fi

CURRENT_BRANCH=$(git symbolic-ref HEAD | sed 's!refs\/heads\/!!')
LATEST_TAG=$(git describe --abbrev=0 --tags)

git checkout -B release-$LATEST_TAG $LATEST_TAG

for build in static ubuntu-12.04 ubuntu-14.04 ubuntu-16.04; do
    github-release upload \
                   --user pbogdan \
                   --repo ngx-top \
                   --tag $LATEST_TAG \
                   --name "ngx-top-$build" \
                   --file build/$build/ngx-top
    github-release upload \
                   --user pbogdan \
                   --repo ngx-top \
                   --tag $LATEST_TAG \
                   --name "ngx-top-gen-$build" \
                   --file build/$build/ngx-top-gen
done

git checkout $CURRENT_BRANCH
