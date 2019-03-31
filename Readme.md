# Haskell project template

Use cabal within a nix shell.

similar to: https://github.com/monadfix/nix-cabal
except I use a makefile and a seperate filewatch script.

Comes with:
+ a poor mans' file watch.
+ a nix shell.
+ A couple of handy make commands.
+ Starting haskell files


## TODO

+ build tools as input of the nix shell (Nobody seems to do this, but it allows a pure shell, and reducing the change of works on my machine)
+ Move towards steel overseer for platform independence
