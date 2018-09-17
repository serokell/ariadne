{ pkgs ? import ./closure.nix }: with pkgs;

let
  project = import ./. { inherit pkgs; };
in

rec {
  ariadne-qt-app-flatpak = buildFlatpak {
    app-id = "io.serokell.ariadne.Qt";
    command = "${project.ariadne-qt-app}/bin/ariadne-qt";
    finish-args = [ "--share=network" ];
  };

  ariadne-vty-app = haskell.lib.justStaticExecutables project.ariadne-vty-app;

  ariadne-vty-app-flatpak = buildFlatpak {
    app-id = "io.serokell.ariadne.VTY";
    command = "${ariadne-vty-app}/bin/ariadne";
    finish-args = [ "--share=network" ];
  };
}
