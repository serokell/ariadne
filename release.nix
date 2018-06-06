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
let
  copyDynamicExecutables = drv: runCommandCC "${drv.name}-bin" {
    buildInputs = [ glibc removeReferencesTo ];
  } ''
    mkdir -p $out/bin
    cp ${drv}/bin/* $out/bin
    mkdir -p $out/lib
    echo moving libs
    for binary in $out/bin/*; do
      for library in $(ldd $binary | awk '{print $3}' | grep lib/ghc); do
        cp -n -L $library $out/lib
        remove-references-to -t $(echo $library | cut -d"/" -f1-4) $out/lib/$(basename $library)
      done
    done
    for elf in $out/bin/* $out/lib/*; do
      chmod +w $elf
      patchelf --set-rpath $(patchelf --print-rpath $elf | tr : \\n | grep -v lib/ghc | tr \\n :)$out/lib $elf
      chmod -w $elf
    done
  '';
  ariadne-bin = haskell.lib.justStaticExecutables (ariadne { withQt = false; });
  ariadne-qt-bin = copyDynamicExecutables (ariadne { withQt = true; });
  # runs in the chroot
  ariadne-run = writeShellScriptBin "run-ariadne.sh" ''
    [ -z "$XDG_CACHE_HOME" ] && export XDG_CACHE_HOME="$HOME/.cache"
    [ -z "$XDG_CONFIG_HOME" ] && export XDG_CONFIG_HOME="$HOME/.config"
    [ -z "$XDG_DATA_HOME" ] && export XDG_DATA_HOME="$HOME/.local/share"

    export CACHE_DIR="$XDG_CACHE_HOME/ariadne"
    export CONFIG_DIR="$XDG_CONFIG_HOME/ariadne"
    export DATA_DIR="$XDG_DATA_HOME/ariadne"
    export PATH="${xsel}/bin:$PATH"

    mkdir -p "$CACHE_DIR" "$DATA_DIR"

    if [ ! -e "$CONFIG_DIR" ]; then
      cp --no-preserve=mode -r "${ariadne-config}" "$CONFIG_DIR"
    fi

    export LOCALE_ARCHIVE="${glibcLocales.override { allLocales = false; }}/lib/locale/locale-archive";
    exec ${ariadne-bin}/bin/ariadne "$@"
  '';
in
(nix-bundle.nix-bootstrap-nix {
  target = ariadne-run;
  run = "/bin/run-ariadne.sh";

  preStart = ''
    case "$(realpath -q /etc/services)" in
      /nix/store/*)
        echo "Ironically, you can't use this bundle on NixOS." >&2
        echo "Use Nix-specific installation instructions instead!" >&2
        exit 1
        ;;
    esac
  '';
}) // { passthru = { inherit ariadne-qt-bin; }; }
