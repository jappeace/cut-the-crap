{
  pkgs ? import ./nix/pin.nix {
  config = import ./nix/config.nix {};
}, ... }:

let
  ignore = import ./nix/gitignoreSource.nix { inherit (pkgs) lib; };
  # https://github.com/NixOS/nixpkgs/blob/dbacb52ad8/pkgs/development/haskell-modules/make-package-set.nix#L216
  cut-the-crap = pkgs.haskellPackages.callCabal2nix "cut-the-crap" (ignore.gitignoreSource ./.) {};
in
# https://github.com/NixOS/nixpkgs/blob/dbacb52ad8/pkgs/development/haskell-modules/generic-builder.nix#L13

with pkgs.haskell.lib;
import nix/overlay/runtimePatch.nix {
  inherit pkgs cut-the-crap overrideCabal addBuildTool self;
}
