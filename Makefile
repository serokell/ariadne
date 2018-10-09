.PHONY: dev dev-core dev-cardano dev-cli dev-vty dev-qt test haddock haddock-no-deps stylish lint clean

CORE_PACKAGES = ariadne-core
CARDANO_PACKAGES = ariadne-cardano
VTY_PACKAGES = ariadne-vty-app
QT_PACKAGES = ariadne-qt-app

# Options for development
STACK_DEV_OPTIONS = --fast --ghc-options -Wwarn --file-watch
# Options to build more stuff (tests and benchmarks)
STACK_BUILD_MORE_OPTIONS = --test --bench --no-run-tests --no-run-benchmarks

# Build everything (including tests and benchmarks) with development options.
dev:
	stack build $(STACK_DEV_OPTIONS) $(STACK_BUILD_MORE_OPTIONS) .

# Build only `ariadne-core` library with development options.
# Useful because logs will be printed.
dev-core:
	stack build $(CORE_PACKAGES) $(STACK_DEV_OPTIONS)

# Build only `ariadne-cardano` library with development options.
# Useful because logs will be printed.
dev-cardano:
	stack build $(CARDANO_PACKAGES) $(STACK_DEV_OPTIONS)

# Build only TUI with development options.
dev-vty:
	stack build $(VTY_PACKAGES) $(STACK_DEV_OPTIONS)

# Build only Qt GUI with development options.
dev-qt:
	stack build $(QT_PACKAGES) $(STACK_DEV_OPTIONS)

# Run tests in all packages which have them.
test:
	stack test knit ariadne-cardano

# Run haddock for all packages.
haddock:
	stack haddock

# Run haddock for all our packages, but not for dependencies.
haddock-no-deps:
	stack haddock --no-haddock-deps

stylish:
	stylish-haskell -i `find ariadne knit ui -iname '*.hs'`

lint:
	scripts/lint.sh

clean:
	stack clean
