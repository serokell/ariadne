let
  nixpkgs = import (fetchTarball {
    url = "https://github.com/kirelagin/nixpkgs/archive/44ce2b7235304dae9a1b3c7d8ab635e6ae6d2305.tar.gz";
    sha256 = "0hsd3c40ykhh7av8i3ayxj57s8z6y0vxrzvss9rh4996qnw13hwx";
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
