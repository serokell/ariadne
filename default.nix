{ pkgs ? import ./nixpkgs.nix }: with pkgs;

let
  stack4nix = fetchGit {
    url = https://github.com/serokell/stack4nix;
    rev = "364663ca8ebf006f3fd9f76daea862afac705c69";
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
