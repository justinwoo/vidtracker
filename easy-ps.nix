{ pkgs ? import <nixpkgs> {}}:

import (pkgs.fetchFromGitHub {
  owner = "justinwoo";
  repo = "easy-purescript-nix";
  rev = "a62866fa1c6493cad5d07123a025347f2a0b6440";
  sha256 = "0hzpnp1vjz58g4awdj2jciw015rg37lsfv9fdawd6z1zqqm0mib0";
})
