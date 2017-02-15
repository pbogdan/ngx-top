#!/bin/bash
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
[ ! -f /usr/local/bin/upx ] && wget https://github.com/lalyos/docker-upx/releases/download/v3.91/upx \
     -O /usr/local/bin/upx && \
    chmod +x /usr/local/bin/upx
cd /tmp/src/ngx-top && \
    stack install --only-dependencies --system-ghc && \
    stack --local-bin-path /build/static/ install --test --system-ghc \
          --ghc-options '-optl-static' && \
    upx -q /build/ngx-top
