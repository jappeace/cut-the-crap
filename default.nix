{ pkgs ? import ./nix/pin.nix {}, ... }:

let
  ignore = import ./nix/gitignoreSource.nix { inherit (pkgs) lib; };
  # https://github.com/NixOS/nixpkgs/blob/dbacb52ad8/pkgs/development/haskell-modules/make-package-set.nix#L216
in
# https://github.com/NixOS/nixpkgs/blob/dbacb52ad8/pkgs/development/haskell-modules/generic-builder.nix#L13
pkgs.haskellPackages.callCabal2nix "cut-the-crap" (ignore.gitignoreSource ./.) {}
