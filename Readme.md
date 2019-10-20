# Haskell project template

Use cabal within a nix shell.

similar to: https://github.com/monadfix/nix-cabal
except I use a makefile and a seperate filewatch script.

Comes with:
+ GHCID for [file watch](https://jappieklooster.nl/ghcid-for-multi-package-projects.html)
+ a nix shell, meaning somewhat platform independence.
  + which is pinned by default
+ A couple of handy make commands.
+ Starting haskell files, assuming we put practically all code in library
+ Working HSpec
