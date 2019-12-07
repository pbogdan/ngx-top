{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc865" }:
let
    inherit (nixpkgs.pkgs.haskell.lib) doJailbreak;
    log-parser-src = (nixpkgs.fetchgit {
      url = "https://github.com/pbogdan/log-parser";
      rev = "4f073681f3714298185cf0077358e45e0c657d45";
      sha256 = "18c4hnv8svpb5gl36j6kvf67zahvi9rqd7ynv83dqwrx6w2s4rhr";
    });
    packageOverrides = super: let self = super.pkgs; in
    {
        haskell = super.haskell // {
            packages = super.haskell.packages // {
                "${compiler}" = super.haskell.packages.${compiler}.override {
                    overrides = self: super: {
                    };
                };
            };
        };
    };
    config =  { inherit packageOverrides; };
    pkgs = (import <nixpkgs> { inherit config; }).pkgs;
in  pkgs.haskell.packages.${compiler}.callPackage ./ngx-top.nix {
    log-parser = import (log-parser-src) { inherit nixpkgs compiler; };
}
