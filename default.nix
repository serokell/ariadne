let
  overlay = import ''${builtins.fetchGit "ssh://git@github.com/serokell/serokell-overlay"}/pkgs'';
  nixpkgs = import (builtins.fetchTarball "https://github.com/serokell/nixpkgs/archive/master.tar.gz") {
    overlays = [ overlay ];
  };
in

with nixpkgs;

let
  closure = (stackClosure haskell.compiler.ghc822 ./.).override {
    overrides = final: previous: with haskell.lib; {
      ariadne-cardano = haskell.lib.doCheck (overrideCabal previous.ariadne-cardano (super: {
        buildTools = [ git ];
      }));

      ariadne-qt = disableLibraryProfiling (overrideCabal previous.ariadne-qt (super: {
        # https://github.com/NixOS/nixpkgs/issues/25585
        # RPATH of binary contains a forbidden reference to /tmp/nix-build...
        preFixup = ''rm -rf "$(pwd)"'';
        enableSharedExecutables = true;
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

{ inherit (closure) ariadne-vty ariadne-qt; }
