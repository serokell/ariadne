with import <nixpkgs> {};

let
  closure = import ./closure.nix { inherit pkgs; };

  depend = master: slave: haskell.lib.overrideCabal master (self: {
    libraryHaskellDepends = self.libraryHaskellDepends ++ [ slave ];
  });

  overrides = final: previous: {
    ariadne = depend previous.ariadne git;
  };
in

(closure.override { inherit overrides; })
