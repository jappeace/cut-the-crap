let 
pinnedPkgs = 
    (builtins.fetchGit {
    # Descriptive name to make the store path easier to identify
    name = "nixos-pin-04.06.2020";
    url = https://github.com/nixos/nixpkgs/;
    rev = "cda41cf743f0942aa3f1833d5e0385ad585fa411";
    }) ;
in
import pinnedPkgs {
    # since I also use this for clients I don't want to have to care
    config.allowUnfree = true; # took me too long to figure out
}
