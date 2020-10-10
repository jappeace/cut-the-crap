{
  config ? import ./nix/config.nix {},
  # https://github.com/NixOS/nixpkgs/blob/master/pkgs/top-level/haskell-packages.nix
  pkgs ? import ./nix/pin.nix {
    inherit config;
} }:

pkgs.haskell.packages.${config.compiler}.shellFor {
  packages = ps : [ ps.cut-the-crap ];
  buildInputs = [
        pkgs.ghcid
        pkgs.cabal-install
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
}
