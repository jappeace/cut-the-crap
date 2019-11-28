{ pkgs ? import ./pin.nix }:

# Moving this out of the normal shell always allows us to generate
# cabal files, even if they're corrupted.
pkgs.mkShell{
    buildInputs = [
        pkgs.cabal2nix 
        pkgs.haskellPackages.hpack
    ];
}
