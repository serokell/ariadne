{ pkgs }:

pkgs.jemalloc.overrideAttrs (_: rec {
  version = "4.5.0";
  name = "jemalloc-4.5.0";
  src = pkgs.fetchurl {
    url = "https://github.com/jemalloc/jemalloc/releases/download/${version}/${name}.tar.bz2";
    sha256 = "10373xhpc10pgmai9fkc1z0rs029qlcb3c0qfnvkbwdlcibdh2cl";
  };
})

