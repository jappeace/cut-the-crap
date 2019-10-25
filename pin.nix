let 
pinnedPkgs = 
    (builtins.fetchGit {
    # Descriptive name to make the store path easier to identify
    name = "nixos-pin-25.10.2019";
    url = https://github.com/nixos/nixpkgs/;
    rev = "77a5bf5bfbacafbafbe2fc7657246ba8557f65ca";
    }) ;
in
import pinnedPkgs {
    # since I also use this for clients I don't want to have to care
    config.allowUnfree = true; # took me too long to figure out
}
