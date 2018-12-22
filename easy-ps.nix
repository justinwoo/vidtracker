{ pkgs ? import <nixpkgs> {}}:

import (pkgs.fetchFromGitHub {
  owner = "justinwoo";
  repo = "easy-purescript-nix";
  rev = "d7c70ed0b4eb10f523f516a36a06eb5cd94633e2";
  sha256 = "1lnz1psckmx8babma769zgnxsn275bq7rf3himk04zvsp7x6d0dx";
})
