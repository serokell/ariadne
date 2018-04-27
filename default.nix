let
  nixpkgs = import "${overlay}/nixpkgs.nix";
  overlay = builtins.fetchGit {
    url = "ssh://git@github.com:/serokell/serokell-ops.git";
    rev = "f8803d3d1f15c714931f46e096ba28d93448e148";
  };
in

with nixpkgs;

buildStack {
  package = "ariadne";
  src = lib.cleanSource ./.;

  overrides = final: previous: with nixpkgs.haskell.lib; {
    ariadne = addBuildTool previous.ariadne git;
    ariadne-qt-ui = disableLibraryProfiling previous.ariadne-qt-ui;
    qtah-cpp = overrideCabal previous.qtah-cpp (self: {
      librarySystemDepends = (self.librarySystemDepends or []) ++ [ nixpkgs.qt5.qtbase ];
    });
    qtah = final.callPackage
      ({ mkDerivation, base, binary, bytestring, Cabal, directory
       , fetchgit, filepath, hoppy-runtime, qtah-cpp, qtah-generator
       , stdenv, qtbase
       }:
      mkDerivation {
        pname = "qtah";
        version = "0.4.0";
        src = fetchgit {
          url = "ssh://git@github.com/serokell/qtah.git";
          sha256 = "0ns7f39say1l50w59n25wf58lwvqakd9q24qghkfy2khp4wz7w52";
          rev = "4486765dac00783f856d25f97dc4e2cb75427d2c";
        };
        postUnpack = "sourceRoot+=/qtah; echo source root reset to $sourceRoot";
        setupHaskellDepends = [ base Cabal directory filepath ];
        libraryHaskellDepends = [
          base binary bytestring hoppy-runtime qtah-cpp qtah-generator
        ];
        libraryToolDepends = [ qtbase ];
        doHaddock = false;
        doCheck = false;
        homepage = "http://khumba.net/projects/qtah";
        description = "Qt bindings for Haskell";
        license = stdenv.lib.licenses.lgpl3;
      }) { inherit (pkgs.qt5) qtbase; };
  };
}
