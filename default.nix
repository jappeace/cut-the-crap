{ pkgs ? import ./pin.nix, ... }:

let
  ignore = import ./nix/gitignoreSource.nix { inherit (pkgs) lib; };
  # https://github.com/NixOS/nixpkgs/blob/dbacb52ad8/pkgs/development/haskell-modules/make-package-set.nix#L216
  cabalBuild = pkgs.haskellPackages.callCabal2nix "cut-the-crap" (ignore.gitignoreSource ./.) {};
in
# https://github.com/NixOS/nixpkgs/blob/dbacb52ad8/pkgs/development/haskell-modules/generic-builder.nix#L13
pkgs.haskell.lib.overrideCabal cabalBuild (drv: {
      libraryToolDepends =  [
        pkgs.haskellPackages.c2hs
        ];
})
