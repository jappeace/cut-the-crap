{compiler ? builtins.readFile ./defaultCompiler, ...}:

{
inherit compiler;
packageOverrides = pkgs: {

    c2hs = pkgs.haskellPackages.c2hs;
    haskell = pkgs.lib.recursiveUpdate pkgs.haskell {
      packageOverrides = hpNew: hpOld: {
          cut-the-crap = hpNew.callPackage ../default.nix {};
        };
    };
};
}
