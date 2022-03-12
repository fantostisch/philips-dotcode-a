.PHONY: build run test check debug watch format

build:
	stack build --fast

run:
	$(MAKE) build
	stack exec dotcode -- $(args)

test:
	stack test

check:
	ormolu -m check app/*.hs src/*/*.hs test/*.hs
	$(MAKE) test

debug:
	stack run --profile -- +RTS -xc -RTS -- $(args)

watch:
	stack build --fast --file-watch

format:
	ormolu -m inplace app/*.hs src/*/*.hs test/*.hs
