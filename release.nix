{ pkgs ? import ./nixpkgs.nix }: with pkgs;

let
  buildFlatpak = callPackage (fetchGit {
    url = "https://github.com/serokell/nix-flatpak";
    rev = "46a2aadf37981d6313621913cd2802debfb763fd";
  }) {};

  project = import ./. { inherit pkgs; };
in

rec {
  inherit project ariadne-qt-app;

  ariadne-qt-app-flatpak = buildFlatpak {
    app-id = "io.serokell.ariadne.Qt";
    command = "${ariadne-qt-app}/bin/ariadne";
    finish-args = [ "--share=network" ];
  };

  ariadne-vty-app = haskell.lib.justStaticExecutables project.ariadne-vty-app;

  ariadne-vty-app-flatpak = buildFlatpak {
    app-id = "io.serokell.ariadne.VTY";
    command = "${ariadne-vty-app}/bin/ariadne";
    finish-args = [ "--share=network" ];
  };
}
