{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }: with pkgs;

let
  jemalloc450 = import ./jemalloc450.nix { inherit pkgs; };
  rocksdb = pkgs.rocksdb.override { jemalloc = jemalloc450; };
  hsPkgs = haskell.packages.ghc802;
in 

haskell.lib.buildStackProject {
  inherit ghc;
  name = "myEnv";
  buildInputs = [ autoreconfHook
                  bsdiff
                  gcc 
                  git 
                  gmp 
                  hsPkgs.cpphs
                  hsPkgs.happy
                  lzma 
                  ncurses 
                  openssl 
                  openssh
                  rocksdb 
                  zlib 
                ];
  buildPhase = ''
    export LANG=en_US.UTF-8
    '';
    }
