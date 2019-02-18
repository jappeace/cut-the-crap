OPTIMIZATION=-O0
build: update-cabal
	cabal new-build all -j --ghc-options $(OPTIMIZATION)

haddock:
	cabal new-haddock all
run:
	make run-in-shell RUN="cabal run"

file-watch: clean
	scripts/watch.sh

update-cabal:
	hpack --force ./
	cabal2nix ./ > default.nix

enter:
	nix-shell --cores 0 -j 8 --pure

RUN=""
run-in-shell:
	nix-shell --cores 0 -j 8 --pure --run "$(RUN)"

clean:
	rm -fR dist dist-*

.PHONY: test
