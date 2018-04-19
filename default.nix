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

  overrides = final: previous: {
    ariadne = dependCabal previous.ariadne [ git ];
  };
}
