{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:
with nixpkgs.pkgs.haskell.lib;
let log-parser-src = (nixpkgs.fetchgit {
      url = "https://github.com/pbogdan/log-parser";
      rev = "2ea5c2c63a483fa245f8202095232945bfb7507b";
      sha256 = "1dwk3sy1ghq4yjdyil33l4kxnx4chy55a1yjnf498yvfiia33s6z";
    });
    packageOverrides = super: let self = super.pkgs; in
    {
        haskell = super.haskell // {
            packages = super.haskell.packages // {
                "${compiler}" = super.haskell.packages.${compiler}.override {
                    overrides = self: super: {
                        streaming = (doJailbreak (self.callHackage "streaming" "0.1.4.5" {}));
                        tailfile-hinotify = (doJailbreak super.tailfile-hinotify);
                    };
                };
            };
        };
    };
    config =  { inherit packageOverrides; };
    pkgs = (import <nixpkgs> { inherit config; }).pkgs;
in  pkgs.haskell.packages.${compiler}.callPackage ./ngx-top.nix {
    log-parser = import (log-parser-src) { };
}
