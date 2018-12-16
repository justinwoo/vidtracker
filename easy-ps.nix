{ pkgs ? import <nixpkgs> {}}:

import (pkgs.fetchFromGitHub {
  owner = "justinwoo";
  repo = "easy-purescript-nix";
  rev = "e9151740dfd0d9d74f75af3d4b8abe940722561e";
  sha256 = "04pxwy2y2c66wj7wqqb3hi83b3f1pnksq9k9jxdwf4p0rv9cs6ag";
})
