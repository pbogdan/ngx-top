#!/bin/bash
cp -r /src/ .
rm -r **/*/.stack-work
apt-get update
DEBIAN_FRONTEND=noninteractive apt-get -y install python-software-properties software-properties-common
add-apt-repository ppa:hvr/ghc
apt-get update
DEBIAN_FRONTEND=noninteractive apt-get -y install git \
    ca-certificates \
    zlib1g-dev \
    libtinfo-dev \
    wget \
    ghc-8.0.2 \
    cabal-install-1.24
[ ! -f /usr/local/bin/stack ] && wget -qO- https://get.haskellstack.org/ | sh
[ ! -f /usr/local/bin/upx ] && wget https://github.com/lalyos/docker-upx/releases/download/v3.91/upx \
     -O /usr/local/bin/upx && \
    chmod +x /usr/local/bin/upx
export PATH=/opt/ghc/bin:$PATH
cd /tmp/src/ngx-top && \
    stack install --only-dependencies --system-ghc && \
    stack --local-bin-path /build/ubuntu-$1 install --test --system-ghc && \
    upx -q /build/ubuntu-$1/ngx-top
