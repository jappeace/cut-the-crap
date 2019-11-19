{ pkgs ? import ./pin.nix }:
let 
    origBuild = pkgs.haskellPackages.callPackage ./default.nix { };
    wTools = pkgs.haskell.lib.overrideCabal origBuild (drv: {
      libraryToolDepends = drv.libraryToolDepends ++ [
        pkgs.ghcid
        pkgs.cabal-install
        pkgs.haskellPackages.hasktags
        pkgs.ffmpeg
        pkgs.hlint
        (import ./jumpcutter.nix)
        ];
    });
in 
wTools.env
