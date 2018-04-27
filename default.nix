let
  nixpkgs = import (fetchTarball {
    url = "https://github.com/kirelagin/nixpkgs/archive/5db888c0cc6a842f6f90858fc0f30843e7804aaf.tar.gz";
    sha256 = "176hn0mc76fhx3y2mqxrifhfw1s5rsw26p4bxrdb43857jxs2z9l";
  }) { overlays = [ (import "${overlay}/pkgs") ]; };
  overlay = builtins.fetchGit {
    url = "ssh://git@github.com:/serokell/serokell-ops.git";
  };
in

with nixpkgs;

buildStack {
  package = "ariadne";
  src = lib.cleanSource ./.;

  overrides = final: previous: {
    ariadne = dependCabal previous.ariadne [ git ];
  };
}
