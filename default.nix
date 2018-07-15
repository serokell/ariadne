let
  nixpkgs = fetchTarball "https://github.com/serokell/nixpkgs/archive/master.tar.gz";
  serokell-overlay = fetchGit "ssh://git@github.com/serokell/serokell-overlay";
in

with import nixpkgs {
  config.allowUnfree = true;
  overlays = [ (import "${serokell-overlay}/pkgs") ];
};

let
  closure = (stackClosure haskell.compiler.ghc822 ./.).override {
    overrides = final: previous: with haskell.lib; {
      ariadne-cardano = haskell.lib.doCheck (overrideCabal previous.ariadne-cardano (super: {
        buildTools = [ git ];
      }));

      ariadne-qt-lib = disableLibraryProfiling (overrideCabal previous.ariadne-qt-lib (super: {
        # https://github.com/NixOS/nixpkgs/issues/25585
        # RPATH of binary contains a forbidden reference to /tmp/nix-build...
        preFixup = ''rm -rf "$(pwd)"'';
        enableSharedExecutables = true;

        # Our custom Setup.hs calls `rcc` from Qt and links to libQt5Core
        librarySystemDepends = [ qt5.qtbase ];
        libraryToolDepends = [ qt5.qtbase ];
      }));

      knit = overrideCabal previous.knit (super: with final; {
        doCheck = true;
        testDepends = [ hspec universum ];
      });

      qtah-cpp = overrideCabal previous.qtah-cpp (super: {
        librarySystemDepends = [ qt5.qtbase ];
      });

      qtah = overrideCabal previous.qtah (super: {
        libraryToolDepends = [ qt5.qtbase ];
      });
    };
  };
in

{ inherit (closure) ariadne-cli-app ariadne-vty-app ariadne-qt-app; }
