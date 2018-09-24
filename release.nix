{ pkgs ? import ./closure.nix }: with pkgs;

let
  project = import ./. { inherit pkgs; };
in

rec {
  ariadne-hlint = runCommand "ariadne-hlint" {} ''
    mkdir $out && cd ${lib.cleanSource ./.}

    ${hlint}/bin/hlint -j --report=$out/ariadne.html --hint .hlint.yaml --hint .hlint-universum.yaml ariadne ui
    ${hlint}/bin/hlint -j --report=$out/knit.html --hint .hlint.yaml knit
  '';

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
