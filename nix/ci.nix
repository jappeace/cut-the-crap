{
  default = import ../default.nix {};

  # this is of course a moving target, but at least the expression evaluates
  # It builds whatever is in nixpkgs, hopefully warning us of breakage
  # but that's dependend on if the pin is being moved.
  # TODO re enable with 1.4.1 release
#   nixpkgs-overlay = (import ./pin.nix {
#     config.allowBroken = true; # we get marked broken if we muck up
#     overlays = [
#       (import ./overlay)
#     ];
#   }).haskellPackages.cut-the-crap;
}
