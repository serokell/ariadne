make pdf:
	pdflatex tex/Auxx.tex

.PHONY: dev test stylish clean

dev:
	stack build ariadne --fast --ghc-options -Wwarn --file-watch

test:
	stack test ii-extras knit ariadne-vty-ui ariadne

stylish:
	stylish-haskell -i `find ariadne knit util ui -iname '*.hs'`

clean:
	stack clean