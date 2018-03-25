#!/bin/bash
set -e
cp -r /src/ .
rm -r **/*/.stack-work
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
stack upgrade
export PATH=$HOME/.local/bin:$PATH
[ ! -f /usr/local/bin/upx ] && wget https://github.com/lalyos/docker-upx/releases/download/v3.91/upx \
     -O /usr/local/bin/upx && \
    chmod +x /usr/local/bin/upx
cd /tmp/src/ngx-top && \
    stack install --only-dependencies --system-ghc --skip-ghc-check && \
    stack --local-bin-path /build/static/ install --test --skip-ghc-check \
    --system-ghc --ghc-options '-optl-static' && \
    upx -q /build/static/ngx-top
