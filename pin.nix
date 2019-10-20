let 
pinnedPkgs = 
    (builtins.fetchGit {
    # Descriptive name to make the store path easier to identify
    name = "nixos-pin-19.10.2019";
    url = https://github.com/nixos/nixpkgs/;
    rev = "f203d50d09052792866f926e7c2d3b3bf76d3388";
    }) ;
in
import pinnedPkgs {
    # since I also use this for clients I don't want to have to care
    config.allowUnfree = true; # took me too long to figure out
}
