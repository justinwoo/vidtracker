let
  pkgs = import <nixpkgs> {};

  easy-ps = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "20ea9074ffa468af60d96f30ea836fa67d8af7b0";
    sha256 = "1kqa7vvy97kl38syjy4m6p254nkswjlbz5prib4xvi0ark1s16g6";
  });

in pkgs.stdenv.mkDerivation {
  name = "easy-purescript";

  buildInputs = easy-ps.buildInputs;
}
