{ compiler ? "ghc865" }:
(import ./default.nix { inherit compiler; }).env
