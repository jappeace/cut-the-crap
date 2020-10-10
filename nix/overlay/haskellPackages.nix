{ pkgs, haskellLib }:

with haskellLib;
self: super:
builtins.intersectAttrs super {
  cut-the-crap = import ./runtimePatch.nix {inherit pkgs overrideCabal addBuildTool self; cut-the-crap = super.cut-the-crap; };
}
