let
  pkgs = import <nixpkgs> {};

  easy-ps = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "43b18de";
    sha256 = "0c36pxafmlyq643kvyq61rw8z3h5dbg5gjb6mi7rxb5bsasqwxp8";
  });

  psc-package2nix = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "psc-package2nix";
    rev = "414ba2f58e270dece3834021e380c41cd940b983";
    sha256 = "0lrw2k1gm4aamnlxi16syibyqi7i3nvx9bwzq889vd1p0sbzxs9x";
  }) {};

in pkgs.stdenv.mkDerivation {
  name = "easy-purescript";
  src = ./.;

  buildInputs = easy-ps.buildInputs ++ [
    psc-package2nix
    pkgs.jq
    pkgs.nix-prefetch-git
  ];
}
