.PHONY: dev test stylish clean

dev:
	stack build ariadne --fast --ghc-options -Wwarn --file-watch

test:
	stack test knit ariadne-vty-ui ariadne

stylish:
	stylish-haskell -i `find ariadne knit ui -iname '*.hs'`

clean:
	stack clean
