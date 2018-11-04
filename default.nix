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
    rev = "409aab26afa0784eb90440da33b1ad4d56aedb93";
    sha256 = "11j7ny8qsv8361h88wrmmmzbhg4m5g9bbg5v3im3mfs0nhhkbrz9";
  }) {};

in pkgs.stdenv.mkDerivation {
  name = "easy-purescript";

  buildInputs = easy-ps.buildInputs ++ [
    psc-package2nix
    pkgs.jq
    pkgs.nix-prefetch-git
  ];
}
