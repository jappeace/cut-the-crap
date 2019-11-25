{ pkgs ? import ./pin.nix }:
let 
    origBuild = import ./default.nix { };
    wTools = pkgs.haskell.lib.overrideCabal origBuild (drv: {
      libraryToolDepends = drv.libraryToolDepends ++ [
        pkgs.ghcid
        pkgs.cabal-install
        pkgs.haskellPackages.hasktags
        ];
    });
in 
wTools.env
