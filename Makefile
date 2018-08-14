.PHONY: dev dev-core dev-cardano dev-cli dev-vty dev-qt test stylish clean

CORE_PACKAGES = ariadne-core
CARDANO_PACKAGES = ariadne-cardano
CLI_PACKAGES = ariadne-cli
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

# Build only CLI with development options.
dev-cli:
	stack build $(CLI_PACKAGES) $(STACK_DEV_OPTIONS)

# Build only TUI with development options.
dev-vty:
	stack build $(VTY_PACKAGES) $(STACK_DEV_OPTIONS)

# Build only Qt GUI with development options.
dev-qt:
	stack build $(QT_PACKAGES) $(STACK_DEV_OPTIONS)

# Run tests in all packages which have them.
test:
	stack test ii-extras knit ariadne-cardano

stylish:
	stylish-haskell -i `find ariadne knit util ui -iname '*.hs'`

clean:
	stack clean
