let
  overlay = import ''${builtins.fetchGit {
    url = "ssh://git@github.com:/serokell/serokell-overlay.git";
    ref = "master";}}/pkgs'';
    nixpkgs = import (builtins.fetchTarball "https://github.com/serokell/nixpkgs/archive/master.tar.gz") {
      overlays = [ overlay ];
  };
  envPaths = [
    "/etc/resolv.conf"
    "/etc/nsswitch.conf"
    "/etc/protocols"
    "/etc/services"
    "$TERMINFO"
    "$LOCALE_ARCHIVE"
  ];
  nix-bundle = import (builtins.fetchGit "https://github.com/serokell/nix-bundle") { inherit nixpkgs; };

  configFiles = nixpkgs.stdenv.mkDerivation {
    name = "ariadne-config-defaults";
    src = nixpkgs.lib.cleanSource ./config;

    installPhase = ''
      cp -r $src $out
    '';
  };

  ariadne = import ./.;
  ariadne-bin = nixpkgs.runCommand "ariadne-bin-${ariadne.version}" {
    inherit (ariadne) version;
  } ''
    install -D ${ariadne}/bin/ariadne $out/bin/ariadne
    cp -r ${configFiles} $out/config
  '';
  runScript = nixpkgs.writeShellScriptBin "run-ariadne.sh" ''
    #!/bin/sh
    set -e
    if [ ! -d "$DATA_DIR/config" ]; then
      cp -r ${ariadne-bin}/config "$DATA_DIR/"
    fi
    exec ${ariadne-bin}/bin/ariadne
  '';
in
with nixpkgs; rec {
  bundle = nix-bundle.nix-bootstrap-nix {
    target = runScript;
    run = "/bin/run-ariadne.sh";
    preStart = ''
      for p in ${nixpkgs.lib.concatMapStringsSep " "
                  (p: nixpkgs.lib.optionalString (p != "") "\"${p}\"") envPaths}; do
        case "$(realpath -q "$p")" in
          /nix/store/*)
            cat <<-EOF
              Ironically, you can't use this nix-bundle on nixos.
              $p symlinks to an entry in the nix store, which we hide.
              Try using the nix-specific installation instructions instead!
  					EOF
            exit 1
            ;;
        esac
      done

      [ -z "$XDG_CACHE_HOME" ] && export XDG_CACHE_HOME="$HOME/.cache"
      [ -z "$XDG_CONFIG_HOME" ] && export XDG_CONFIG_HOME="$HOME/.config"
      [ -z "$XDG_DATA_HOME" ] && export XDG_DATA_HOME="$HOME/.local/share"

      export CACHE_DIR="$XDG_CACHE_HOME/ariadne"
      export CONFIG_DIR="$XDG_CONFIG_HOME/ariadne"
      export DATA_DIR="$XDG_DATA_HOME/ariadne"

      mkdir -p "$CACHE_DIR" "$CONFIG_DIR" "$DATA_DIR"
      chmod 700 "$CACHE_DIR" "$CONFIG_DIR" "$DATA_DIR"

      export LOCALE_ARCHIVE="${nixpkgs.glibcLocales.override { allLocales = false; }}/lib/locale/locale-archive";
    '';
    workingDir = "$DATA_DIR";
  };
}
