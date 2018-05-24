let
overlay = import ''${builtins.fetchGit {
  url = "ssh://git@github.com:/serokell/serokell-overlay.git";
  ref = "master";}}/pkgs'';
  nixpkgs = import (builtins.fetchTarball "https://github.com/serokell/nixpkgs/archive/master.tar.gz") {
    overlays = [ overlay ];
};
ariadne = import ./.;
nix-bundle = import (builtins.fetchGit "https://github.com/serokell/nix-bundle") { inherit nixpkgs; };
in
  with nixpkgs;
rec {5
  ariadne-bin = runCommand "ariadne-bin-${ariadne.version}" {
    inherit (ariadne) version;
  } ''
    install -D ${ariadne}/bin/ariadne $out/bin/ariadne
  '';
  bundle = nix-bundle.nix-bootstrap-nix {
    target = ariadne-bin;
    run = "/bin/ariadne";
    preStart = ''
      echo "using working directory ~/ariadne"
      mkdir -p $HOME/ariadne
      export LOCALE_ARCHIVE="${nixpkgs.glibcLocales.override { allLocales = false; }}/lib/locale/locale-archive";
    '';
    workingDir = "$HOME/ariadne";
  };
}
# extraPaths = [
#   "/etc/resolv.conf"
#   "/etc/nsswitch.conf"
#   "/etc/protocols"
#   "/etc/services"
#   $TERMINFO
#   $LOCALE_ARCHIVE
# ]
