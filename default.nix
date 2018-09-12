{ pkgs ? import ./nixpkgs.nix }: with pkgs;

let
  stack4nix = fetchGit {
    url = https://github.com/serokell/stack4nix;
    rev = "dee5f58317a0f067c6adfceed4d710e53d56cac5";
  };

  overrides = final: previous: with haskell.lib; {
    ariadne-cardano = overrideCabal previous.ariadne-cardano (super: {
      buildTools = [ git ];
      doHaddock = false; 
    });

    ariadne-qt-lib = overrideCabal previous.ariadne-qt-lib (super: {
      libraryToolDepends = [ qt5.qtbase ];
      librarySystemDepends = [ qt5.qtbase ];
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
