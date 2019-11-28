{ pkgs ? import ./pin.nix }:

pkgs.mkShell{
    buildInputs = [
        pkgs.hlint
        pkgs.haskellPackages.brittany
        pkgs.findutils
    ];
}
