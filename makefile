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
	cabal2nix . > dependencies.nix

enter:
	nix-shell --cores 0 -j 8 --pure

RUN=""
run-in-shell:
	nix-shell --cores 0 -j 8 --run "$(RUN)"

WORK="/tmp/cut-the-crap"
clean-work-dir:
	rm -fR $(WORK)
	mkdir -p $(WORK)

runDebug: clean-work-dir
	cabal new-run cut-the-crap --ghc-options $(OPTIMIZATION) -- \
		--inFile ./input.mkv \
		--outFile out.mkv \
		--voiceTrack 1 \
		--workDir $(WORK)

runMusic: 
	nix-shell --run "./run-music.sh ~/streams/"

run: hpack 
	nix-shell --run "make runDebug"

clean: clean-work-dir
	rm -fR dist dist-*

.PHONY: test run_

sdist:
	make run-in-shell RUN="cabal sdist"

brittany_:
	$(shell set -x; for i in `fd hs`; do hlint --refactor --refactor-options=-i $$i; brittany --write-mode=inplace $$i; done)

brittany:
	nix-shell ./travis-shell.nix --run "make brittany_"

