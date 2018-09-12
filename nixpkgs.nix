let
  nixpkgs = fetchGit {
    url = https://github.com/serokell/nixpkgs;
    ref = "20180912.032605";
    rev = "ce5262799647e9e3ce5ea5a9272f915372282c36";
  };
in

import nixpkgs {}
