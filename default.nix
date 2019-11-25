{ pkgs ? import ./pin.nix }:

pkgs.haskellPackages.callPackage ./dependencies.nix { }
