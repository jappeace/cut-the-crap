{ pkgs ? import ./pin.nix }:
let 
    build = pkgs.haskellPackages.callPackage ./default.nix { };
in 
build.env
