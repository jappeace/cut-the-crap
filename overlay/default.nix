self: super:
{
  cut-the-crap = super.haskellPackages.callPackage ./cut-the-crap.nix {};
}
