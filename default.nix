let
  nixpkgs = import "${overlay}/nixpkgs.nix";
  overlay = builtins.fetchGit "ssh://git@github.com:/serokell/serokell-overlay.git";
in

with nixpkgs;

buildStackApplication {
  package = "ariadne";
  src = lib.cleanSource ./.;

  overrides = final: previous: with nixpkgs.haskell.lib; {
    ariadne = overrideCabal previous.ariadne (drv: {
      buildTools = (drv.buildTools or []) ++ [ git ];
      # https://github.com/NixOS/nixpkgs/issues/25585
      preFixup = ''rm -rf "$(pwd)" '';
    });
    ariadne-qt-ui = disableLibraryProfiling previous.ariadne-qt-ui;
    qtah-cpp = overrideCabal previous.qtah-cpp (self: {
      librarySystemDepends = (self.librarySystemDepends or []) ++ [ nixpkgs.qt5.qtbase ];
    });
    qtah = overrideCabal previous.qtah (self: {
      libraryToolDepends = (self.libraryToolDepends or []) ++ [ nixpkgs.qt5.qtbase ];
    });
  };
}
