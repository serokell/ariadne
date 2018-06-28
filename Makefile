.PHONY: dev dev-tui dev-qt test stylish clean

ARIADNE_PACKAGES = ariadne
VTY_PACKAGES = ariadne-vty
QT_PACKAGES = ariadne-qt

STACK_DEV_OPTIONS = --fast --ghc-options -Wwarn --file-watch

dev:
	stack build $(ARIADNE_PACKAGES) $(VTY_PACKAGES) $(QT_PACKAGES) $(STACK_DEV_OPTIONS)

dev-lib:
	stack build $(ARIADNE_PACKAGES) $(STACK_DEV_OPTIONS)

dev-vty:
	stack build $(ARIADNE_PACKAGES) $(VTY_PACKAGES) $(STACK_DEV_OPTIONS)

dev-qt:
	stack build $(ARIADNE_PACKAGES) $(QT_PACKAGES) $(STACK_DEV_OPTIONS)

test:
	stack test ii-extras knit ariadne-vty-ui ariadne

stylish:
	stylish-haskell -i `find ariadne knit util ui -iname '*.hs'`

clean:
	stack clean
