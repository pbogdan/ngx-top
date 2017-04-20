{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
let log-parser-src = (nixpkgs.fetchgit {
  url = "https://github.com/pbogdan/log-parser";
  rev = "d130df9b01ffe17a6694fe1c8482eda537937d9a";
});
in nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./ngx-top.nix {
  log-parser = import (log-parser-src) { };
}
