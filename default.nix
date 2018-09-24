{ pkgs ? import ./closure.nix, shell ? false }: with pkgs;

stackToNix {
  # TODO: implement filtering in stack-to-nix
  root = lib.cleanSource ./.;

  inherit shell;

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
}
