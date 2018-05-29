let
  overlay = import ''${builtins.fetchGit "ssh://git@github.com:/serokell/serokell-overlay.git"}/pkgs'';
  nixpkgs = import (builtins.fetchTarball "https://github.com/serokell/nixpkgs/archive/master.tar.gz") {
    overlays = [ overlay ];
  };
  
  # TODO: this should rather go into the overlay
  nix-bundle = import (builtins.fetchGit "https://github.com/serokell/nix-bundle") { inherit nixpkgs; };

  ariadne-config = with nixpkgs.lib; toDerivation (cleanSource ./config);
  ariadne = import ./.;
in

with nixpkgs;

nix-bundle.nix-bootstrap-nix {
  target = ariadne;
  run = "/bin/ariadne";
  
  preStart = ''
    case "$(realpath -q /etc/services)" in
      /nix/store/*)
        echo "Ironically, you can't use this bundle on NixOS." >&2
        echo "Use Nix-specific installation instructions instead!" >&2
        exit 1
        ;;
    esac

    [ -z "$XDG_CACHE_HOME" ] && export XDG_CACHE_HOME="$HOME/.cache"
    [ -z "$XDG_CONFIG_HOME" ] && export XDG_CONFIG_HOME="$HOME/.config"
    [ -z "$XDG_DATA_HOME" ] && export XDG_DATA_HOME="$HOME/.local/share"

    export CACHE_DIR="$XDG_CACHE_HOME/ariadne"
    export CONFIG_DIR="$XDG_CONFIG_HOME/ariadne"
    export DATA_DIR="$XDG_DATA_HOME/ariadne"

    mkdir -p "$CACHE_DIR" "$DATA_DIR"

    if [ ! -e "$CONFIG_DIR" ]; then
      cp --no-preserve=mode -r ${ariadne-config} "$CONFIG_DIR"
    fi

    export LOCALE_ARCHIVE="${glibcLocales.override { allLocales = false; }}/lib/locale/locale-archive";
  '';
    
  workingDir = "$DATA_DIR";
}
