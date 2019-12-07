let
  fix = f: let x = f x; in x;
in
{ sources ? fix (
    self:
      (import ./nix/sources.nix) // (if static then { nixpkgs = self.nixpkgs-haskell-static; } else {})
  )
, compiler ? "ghc865"
, static ? false
}:
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
    pkgs = (
      import sources.nixpkgs {
        overlays = [
          overlay
        ];
      }
    ).${if static then "pkgsMusl" else "pkgs"};
    ngx-top = pkgs.haskell.packages.${compiler}.callPackage ./ngx-top.nix {};
  in
    pkgs.haskell.lib.overrideCabal ngx-top (
      drv:
        if static then {
          configureFlags = drv.configureFlags or [] ++ [
            "--ghc-option=-optl=-static"
            "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
            "--extra-lib-dirs=${pkgs.ncurses.override { enableStatic = true; }}/lib"
            "--extra-lib-dirs=${pkgs.zlib.static}/lib"
            "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
          ];
          enableSharedExecutables = false;
          enableSharedLibraries = false;
        }
        else {}
    )
