{ pkgs ? import ./pin.nix }:

# Tools we need for testing if the source code is sane.
# Not neccisarly needed for the project
pkgs.mkShell{
    buildInputs = [
        pkgs.hlint
        pkgs.haskellPackages.brittany
        pkgs.findutils
        pkgs.fd
        pkgs.haskellPackages.apply-refact
    ];
}
