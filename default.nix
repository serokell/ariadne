{ pkgs ? import ./nixpkgs.nix }: with pkgs;

let
  stack4nix = fetchGit {
    url = https://github.com/serokell/stack4nix;
    rev = "9c8607c95b4a01a9587846d28cb66e35bd3c37f3";
  };

  overrides = final: previous: with haskell.lib; {
    ariadne-cardano = overrideCabal previous.ariadne-cardano (super: {
      buildTools = [ git ];
    });

    ariadne-qt-lib = overrideCabal previous.ariadne-qt-lib (super: {
      libraryToolDepends = (super.libraryToolDepends or []) ++ [ qt5.qtbase ];
      librarySystemDepends = (super.librarySystemDepends or []) ++ [ qt5.qtbase ];
    });

    qtah = overrideCabal previous.qtah (super: {
      libraryToolDepends = with qt5; [ qtbase qttools ];
    });

    qtah-cpp = overrideCabal previous.qtah-cpp (super: {
      librarySystemDepends = with qt5; [ qtbase qttools ];
    });
  };

  buildStackProject = import stack4nix { inherit pkgs overrides; };
in

buildStackProject ./.
