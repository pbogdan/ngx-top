#!/bin/bash
cp -r /src/ .
rm -r **/*/.stack-work
apk update
apk update
apk add alpine-sdk \
    git \
    ca-certificates \
    ghc \
    gmp-dev \
    zlib-dev \
    cabal stack \
    ncurses-dev \
    ncurses-static
cd /tmp/src/ngx-top && \
    stack install --only-dependencies --system-ghc && \
    stack --local-bin-path /build/ install --test --system-ghc \
    --ghc-options '-optl-static'
