OPTIMIZATION=-O0
build: 
	cabal new-build all -j --ghc-options $(OPTIMIZATION)

haddock:
	cabal new-haddock all

hpack:
	nix-shell ./hpack-shell.nix --run "make update-cabal"

ghcid: clean hpack etags
	nix-shell --run "ghcid -s \"import Main\" -c \"cabal new-repl\" -T \"main\" test:unit"

ghci:
	nix-shell --run "cabal new-repl test:unit"

etags:
	nix-shell --run "hasktags  -e ./src"

update-cabal:
	hpack --force ./
	cabal2nix ./ > default.nix

enter:
	nix-shell --cores 0 -j 8 --pure

RUN=""
run-in-shell:
	nix-shell --cores 0 -j 8 --run "$(RUN)"

clean:
	rm -fR dist dist-*

.PHONY: test

sdist:
	make run-in-shell RUN="cabal sdist"

run_:
	cabal new-run exe --ghc-options $(OPTIMIZATION) -- \
	    # whatever option to haskell program

run:
	nix-shell --run "make run_"
