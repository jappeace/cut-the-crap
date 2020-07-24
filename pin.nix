let 
pinnedPkgs = 
    (builtins.fetchGit {
    # Descriptive name to make the store path easier to identify
    name = "nixos-pin-24.07.20202";
    url = https://github.com/nixos/nixpkgs/;
    rev = "dbacb52ad85216c25567978f7f968c8856b5e686";
    }) ;
in
import pinnedPkgs {
    # since I also use this for clients I don't want to have to care
    config.allowUnfree = true; # took me too long to figure out
}
