{ sources ? import ./nix/sources.nix, compiler ? "ghc865" }:
let
  overlay = self: super: {
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        "${compiler}" = super.haskell.packages.${compiler}.override {
          overrides = hself: hsuper: {
            log-parser = import sources.log-parser {
              inherit sources compiler;
            };
          };
        };
      };
    };
  };
  pkgs = import sources.nixpkgs {
    overlays = [
      overlay
    ];
  };
in pkgs.haskell.packages.${compiler}.callPackage ./ngx-top.nix { }
