{ pkgs ? import <nixpkgs> {}}:

import (pkgs.fetchFromGitHub {
  owner = "justinwoo";
  repo = "easy-purescript-nix";
  rev = "8bfaa045ff37a81e90d66b93f4e6d68475e76cc2";
  sha256 = "00nbwf6jvkpy7892k29fvxwxmyv0511scakkd2ja8d9q913chbdw";
})
