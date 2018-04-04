with import ''${builtins.fetchGit "ssh://git@github.com:/serokell/serokell-ops.git"}/nixpkgs.nix'';

let
  nix-prefetch-git = writeShellScriptBin "nix-prefetch-git" (builtins.readFile ./nix-prefetch-git);
  nix-prefetch-url = stdenv.mkDerivation {
    name = "nix-prefetch-url";
    buildInputs = [ nix pkgconfig ];

    buildCommand = ''
      mkdir -p $out/bin
      cc ${./nix-prefetch-url.cc} -D SYSTEM=\"x86_64-linux\" -o $out/bin/nix-prefetch-url $(pkg-config --cflags --libs nix-expr nix-main nix-store)
    '';
  };

  drv = stdenv.mkDerivation {
    name = "ariadne";
    src = stdenv.lib.cleanSource ./.;

    preferLocalBuild = true;

    LANG = "en_US.UTF-8";
    LOCALE_ARCHIVE = "${glibcLocales}/lib/locale/locale-archive";
    SSL_CERT_FILE = "${cacert}/etc/ssl/certs/ca-bundle.crt";

    buildInputs = [
      cabal-install
      git
      haskell.compiler.ghc802
      nix-prefetch-git
      nix-prefetch-url
      nix
      stack2nix-unwrapped
      strace
    ];

    buildPhase = "HOME=$(pwd) stack2nix . > closure.nix";
    installPhase = "mkdir -p $out && mv * $out";
  };
in

(import "${drv}/ci.nix").ariadne
