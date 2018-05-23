let
  overlay = import ''${builtins.fetchGit "ssh://git@github.com:/serokell/serokell-overlay.git"}/pkgs'';
  nixpkgs = import (builtins.fetchTarball "https://github.com/serokell/nixpkgs/archive/master.tar.gz") {
    overlays = [ overlay ];
  };
in

with nixpkgs;

buildStack {
  package = "ariadne";
  src = lib.cleanSource ./.;

  overrides = final: previous: with haskell.lib; {
    ariadne = overrideCabal previous.ariadne (drv: {
      buildTools = (drv.buildTools or []) ++ [ git ];
      # https://github.com/NixOS/nixpkgs/issues/25585
      preFixup = ''rm -rf "$(pwd)"'';
    });
    ariadne-qt-ui = disableLibraryProfiling previous.ariadne-qt-ui;
    qtah-cpp = overrideCabal previous.qtah-cpp (self: {
      librarySystemDepends = (self.librarySystemDepends or []) ++ [ qt5.qtbase ];
    });
    qtah = overrideCabal previous.qtah (self: {
      libraryToolDepends = (self.libraryToolDepends or []) ++ [ qt5.qtbase ];
    });
  };
}
