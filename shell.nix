{ pkgs ? (
   let 
    hostPkgs = import <nixpkgs> {};
    pinnedPkgs = hostPkgs.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs-channels";
      # nixos-unstable as of 18.02.2019
      rev = "36f316007494c388df1fec434c1e658542e3c3cc";
      sha256 = "1w1dg9ankgi59r2mh0jilccz5c4gv30a6q1k6kv2sn8vfjazwp9k";
    };
  in
  import pinnedPkgs {}
 )
}:
let 
    build = pkgs.haskellPackages.callPackage ./default.nix { };
    buildInputs = [
        pkgs.haskellPackages.hpack
        pkgs.haskellPackages.hspec-discover
        pkgs.cabal-install
        pkgs.ghc
        pkgs.cabal2nix
    ] ++ build.buildInputs;
in 
pkgs.mkShell {
  name = "template";
  inherit buildInputs;
  # allow cabal to discover native inputs
  shellHook = ''
       export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH
       export LANG=en_US.UTF-8
  '';
    
}
