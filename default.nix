let
  pkgs = import <nixpkgs> {};

  easy-ps = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "dac3520da91bf1b2d152d468700b75be5599b784";
    sha256 = "02lcmsscbq1k3c8ap03xxbrf4vbwi1al6hsvfsr3sry7xj8f7ca4";
  });

in pkgs.stdenv.mkDerivation {
  name = "easy-purescript";

  buildInputs = easy-ps.buildInputs ++ [
    pkgs.jq
    pkgs.nix-prefetch-git
  ];
}
