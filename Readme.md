# Haskell project template

Using cabal within a nix shell.
If you like nix this is a good way of doing haskell dev.

similar to: https://github.com/monadfix/nix-cabal
except I use a makefile and ghcid.

Comes with:
+ [GHCID](https://jappieklooster.nl/ghcid-for-multi-package-projects.html)
+ a nix shell, meaning somewhat platform independence.
  + which is pinned by default
+ A couple of handy make commands.
+ Starting haskell files, assuming we put practically all code in library
+ Working HSpec, The detection macro will pickup any file ending with Spec.hs
