{
  config ? import ./nix/config.nix {},
  # https://github.com/NixOS/nixpkgs/blob/master/pkgs/top-level/haskell-packages.nix
  pkgs ? import ./nix/pin.nix {
    inherit config;
} }:

#  https://input-output-hk.github.io/haskell.nix/tutorials/development/
pkgs.haskellPackages.shellFor {
  packages = ps : [ ps.cut-the-crap ];
  buildInputs = [
        pkgs.ghcid
        pkgs.haskellPackages.hasktags
        pkgs.ffmpeg
        pkgs.hlint
        pkgs.pocketsphinx
        pkgs.pkg-config
        pkgs.cabal2nix
        pkgs.haskellPackages.hpack
        pkgs.haskellPackages.c2hs
        ];
  exactDeps = true;
  NIX_PATH="nixpkgs=${pkgs.path}:.";
}
