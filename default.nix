{ pkgs ? import ./nix/pin.nix {}, ... }:

let
  ignore = import ./nix/gitignoreSource.nix { inherit (pkgs) lib; };
  # https://github.com/NixOS/nixpkgs/blob/dbacb52ad8/pkgs/development/haskell-modules/make-package-set.nix#L216

  cut-the-crap = pkgs.haskellPackages.callCabal2nix "cut-the-crap" (ignore.gitignoreSource ./.) {};
  path = pkgs.stdenv.lib.makeBinPath [ pkgs.ffmpeg ];
  hlib = pkgs.haskell.lib;
in
# https://github.com/NixOS/nixpkgs/blob/dbacb52ad8/pkgs/development/haskell-modules/generic-builder.nix#L13

hlib.overrideCabal (hlib.addBuildTool cut-the-crap pkgs.makeWrapper) (_drv: {
    postInstall = ''
      wrapProgram $out/bin/cut-the-crap \
      --prefix PATH : "${path}"
    '';
})
