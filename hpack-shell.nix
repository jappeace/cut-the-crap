{ pkgs ? import ./pin.nix }:

pkgs.mkShell{
    buildInputs = [
        pkgs.cabal2nix 
        pkgs.haskellPackages.hpack
    ];
}
