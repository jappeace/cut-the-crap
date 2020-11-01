{ program ? import ../default.nix { }, run ? "/bin/cut-the-crap" }:
let
nix-bundle-src = builtins.fetchGit {
    url = "https://github.com/matthewbauer/nix-bundle";
    rev = "83742ac8ea4230ca239f948a6b2127449b25ece8";
};
nix-bundle = (import ("${nix-bundle-src}/appimage-top.nix") {}) // (import "${nix-bundle-src}/default.nix" {});
in
   nix-bundle.nix-bootstrap {
      extraTargets = [];
      target = program;
      inherit run;
    }
