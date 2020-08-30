self: super:
{
  # mimic how we have it in nixpkgs
  haskellPackages = super.haskellPackages.override {
    overrides = import ./haskellPackages.nix { pkgs=super; haskellLib=super.haskell.lib;};
  };
}
