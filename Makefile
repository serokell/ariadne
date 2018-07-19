.PHONY: dev dev-lib dev-tui dev-qt test stylish clean

ARIADNE_PACKAGES = ariadne
VTY_PACKAGES = ariadne-vty
QT_PACKAGES = ariadne-qt

# Options for development
STACK_DEV_OPTIONS = --fast --ghc-options -Wwarn --file-watch
# Options to build more stuff (tests and benchmarks)
STACK_BUILD_MORE_OPTIONS = --test --bench --no-run-tests --no-run-benchmarks

# Build everything (including tests and benchmarks) with development options.
dev:
	stack build $(STACK_DEV_OPTIONS) $(STACK_BUILD_MORE_OPTIONS) .

# Build only `ariadne` library with development options.
# Useful because logs will be printed.
dev-lib:
	stack build $(ARIADNE_PACKAGES) $(STACK_DEV_OPTIONS)

# Build only TUI with development options.
dev-vty:
	stack build $(VTY_PACKAGES) $(STACK_DEV_OPTIONS)

# Build only Qt GUI with development options.
dev-qt:
	stack build $(QT_PACKAGES) $(STACK_DEV_OPTIONS)

test:
	stack test ii-extras knit ariadne-vty-ui ariadne

stylish:
	stylish-haskell -i `find ariadne knit util ui -iname '*.hs'`

clean:
	stack clean
