let
  pkgs = import <nixpkgs> {};

  easy-ps = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "8899121af7ad2a92340d67ef7c0cf2cf03297a2a";
    sha256 = "0pi5l9ycmfnqyzkwh6l4b5gsas0kl5jvkgb5b4ip8x8plk6aclmp";
  });

in pkgs.stdenv.mkDerivation {
  name = "easy-purescript";

  buildInputs = easy-ps.buildInputs ++ [
    pkgs.jq
    pkgs.nix-prefetch-git
  ];
}
