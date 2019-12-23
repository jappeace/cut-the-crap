#! /bin/bash

set -xe

stack build
cp -f .stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/cut-the-crap/cut-the-crap ppa/bin
dpkg-deb --build "$HOME"/ppa .
