{ pkgs ? import <nixpkgs> {} }:

let
  easy-ps = import (
    pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "b2efbe30c55ffa16dd6f85cb7c71f77ac3136aa0";
      sha256 = "0mmjvqpkns964sjkbw0waqb47vfhg9r0fp9y0b5pizpikmw3fbp2";
    }
  ) {
    inherit pkgs;
  };

in
pkgs.mkShell {
  buildInputs = [ easy-ps.purs easy-ps.spago easy-ps.spago2nix pkgs.cacert ];
}
