OPTIMIZATION=-O0
build: 
	nix-shell --run "make build_"
build_: 
	cabal new-build all -j --ghc-options $(OPTIMIZATION)

haddock:
	nix-shell --run "cabal new-haddock all"

haddock-hackage:
	cabal new-haddock all --haddock-for-hackage --haddock-option=--hyperlinked-source
	echo "the hackage ui doesn't accept the default format, use command instead"
	cabal upload -d --publish ./dist-newstyle/*-docs.tar.gz

hpack:
	nix-shell ./nix/hpack-shell.nix --run "make update-cabal"

ghcid: clean hpack etags
	nix-shell --run "make ghcid_"

ghcid_:
	ghcid -s "import Main" -c "cabal new-repl" -T "main" test:unit

ghci:
	nix-shell --run "cabal new-repl test:unit"

etags:
	nix-shell --run "hasktags  -e ./src"

update-cabal:
	cat package.yaml.template | sed s,REPLACED_MODEL,"$(MODEL)",g > package.yaml
	hpack --force ./
	rm package.yaml

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
		./input.mkv \
		--workDir $(WORK)

runMusic: 
	nix-shell --run "./run-music.sh ~/streams/"

run: hpack 
	nix-shell --run "make runDebug"

clean: clean-work-dir
	rm -fR dist dist-*

.PHONY: test run_

sdist: hpack
	make static-linked-build
	make run-in-shell RUN="cabal sdist"

brittany_:
	$(shell set -x; for i in `fd hs`; do hlint --refactor --refactor-options=-i $$i; brittany --write-mode=inplace $$i; done)

brittany:
	nix-shell ./nix/travis-shell.nix --run "make brittany_"

# this doesn't really work well, we need to keep updating everything
# according to how ubuntu builds. It's too much work for me
ubuntu-release:
	docker build -t ubuntu-release scripts
	docker run -v ~/projects/cut-the-crap:/home/jappie -t ubuntu-release

static-linked-build:
	nix-build --no-link

MODEL=$(shell nix-shell --run "pkg-config --variable=modeldir pocketsphinx")
sphinx:
	gcc -o run-sphinx.bin main.c includes/speech_recognition.c \
            -Iincludes \
	    -DMODELDIR="\"$(MODEL)\"" \
	    $(shell pkg-config --cflags --libs pocketsphinx sphinxbase)

.PHONY: sphinx
