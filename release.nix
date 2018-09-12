{ pkgs ? import ./nixpkgs.nix }: with pkgs;

let
  buildFlatpak = callPackage (fetchGit {
    url = "https://github.com/serokell/nix-flatpak";
    rev = "46a2aadf37981d6313621913cd2802debfb763fd";
  }) {};

  project = import ./. { inherit pkgs; };
in

rec {
  ariadne-vty-app = haskell.lib.justStaticExecutables project.ariadne-vty-app;

  ariadne-vty-app-flatpak = buildFlatpak {
    app-id = "io.serokell.ariadne.VTY";
    command = "${ariadne-vty-app}/bin/ariadne-vty-app";
    finish-args = [ "--share=network" ];
  };
}
