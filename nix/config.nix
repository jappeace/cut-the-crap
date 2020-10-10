{compiler ? builtins.readFile ./defaultCompiler, ...}:

{
inherit compiler;
packageOverrides = pkgs: {

    haskell = pkgs.lib.recursiveUpdate pkgs.haskell {
      packageOverrides = hpNew: hpOld: {
          cut-the-crap = hpNew.callPackage ../default.nix {};
        };
    };
};
}
