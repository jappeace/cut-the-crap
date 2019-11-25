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
	cabal2nix ./ > dependencies.nix

enter:
	nix-shell --cores 0 -j 8 --pure

RUN=""
run-in-shell:
	nix-shell --cores 0 -j 8 --run "$(RUN)"

run_:
	cabal new-run exe  --ghc-options $(OPTIMIZATION) -- \
		--inFile "out.mkv" \
		--outFile "edited.mkv" \
		--voiceTrack 2 \
		--musicPath "onepiece.mp3"


run: hpack 
	nix-shell --run "make run_"

clean:
	rm -fR dist dist-*

.PHONY: test run_

sdist:
	make run-in-shell RUN="cabal sdist"
