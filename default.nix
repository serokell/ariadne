let
  nixpkgs = import (fetchTarball {
    url = "https://github.com/kirelagin/nixpkgs/archive/4d6a0aba0378c1e4a692f6c0a2647b29dde024b2.tar.gz";
    sha256 = "0j76hjclkn3cjf6kb0w5ywl2asa31qq8010nldgsv5kgsc9lvf9x";
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
