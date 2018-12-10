{ pkgs ? import ./pkgs.nix }: with pkgs;

stdenv.mkDerivation rec {
  name = "ariadne";

  nativeBuildInputs = [
    git
    haskell.compiler.ghc822 # NB: update on LTS changes!
    haskellPackages.cpphs
    haskellPackages.happy
    pkgconfig
  ];

  buildInputs = [
    gmp
    icu
    lzma
    ncurses
    openssl
    qt5.qtbase
    rocksdb
    zlib
  ];

  shellHook = ''
    export LD_LIBRARY_PATH=${lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH
    export LANG=en_US.UTF-8
  '';

  LOCALE_ARCHIVE = lib.optionalString stdenv.isLinux
    "${pkgs.glibcLocales}/lib/locale/locale-archive";

  # NB: on Nixpkgs repin, update this path with new Qt version!
  # ls $(nix-build pkgs.nix -A qt5.qtbase.bin --no-out-link)/lib
  QT_QPA_PLATFORM_PLUGIN_PATH = "${qt5.qtbase.bin}/lib/qt-5.11/plugins/platforms";
}
