{ pkgs ? import ./pin.nix }:
let 
    origBuild = pkgs.haskellPackages.callPackage ./default.nix { };
    wTools = pkgs.haskell.lib.overrideCabal origBuild (drv: {
        libraryToolDepends = drv.libraryToolDepends ++ [ pkgs.ghcid ];
    });
in 
wTools.env
