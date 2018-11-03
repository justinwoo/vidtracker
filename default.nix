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
    rev = "a99092bfe9c32e0df9a826ccc509bd02b7b192c1";
    sha256 = "0296jb8b47zxnnhrjp4ky1mj835jff59sx4przix652mclsy13r3";
  }) {};

in pkgs.stdenv.mkDerivation {
  name = "easy-purescript";

  buildInputs = easy-ps.buildInputs ++ [
    psc-package2nix
    pkgs.jq
    pkgs.nix-prefetch-git
  ];
}
