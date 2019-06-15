let 
hostPkgs = import <nixpkgs> {};
pinnedPkgs = hostPkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channels";
    # nixos-unstable as of 15.06.2019
    rev = "1601f559e89ba71091faa26888711d4dd24c2d4d";
    sha256 = "0iayyz9617mz6424spwbi9qvmcl8hiql42czxg8mi4ycq4p1k0dx";
};
in
import pinnedPkgs {
    # since I also use this for clients I don't want to have to care
    config.allowUnfree = true; # took me to long to figure out
}
