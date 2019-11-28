{ pkgs ? import ./pin.nix }:

pkgs.mkShell{
    buildInputs = [
        pkgs.hlint
        pkgs.cachix
        pkgs.haskellPackages.brittany
    ];
}
