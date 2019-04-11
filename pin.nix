let 
hostPkgs = import <nixpkgs> {};
pinnedPkgs = hostPkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channels";
    # nixos-unstable as of 18.02.2019
    rev = "36f316007494c388df1fec434c1e658542e3c3cc";
    sha256 = "1w1dg9ankgi59r2mh0jilccz5c4gv30a6q1k6kv2sn8vfjazwp9k";
};
in
import pinnedPkgs {
    # since I also use this for clients I don't want to have to care
    config.allowUnfree = true; # took me to long to figure out
}
