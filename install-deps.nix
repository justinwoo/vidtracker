#!/usr/bin/env nix-shell
#!nix-shell --run exit

let
  pkgs = import <nixpkgs> {};

  # import our packages
  packages = import ./packages.nix {};

  # these are the package derivations we will want to work with
  packageDrvs = builtins.attrValues packages.inputs;

  # these are some utils for working with pp2n
  pp2n-utils = import (pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/justinwoo/psc-package2nix/409aab26afa0784eb90440da33b1ad4d56aedb93/utils.nix";
    sha256 = "0rkqisfvpz5x8j2p0llv0yzgz5vnzy7fcfalp8nkymbshk8702gg";
  });

in pkgs.stdenv.mkDerivation {
  name = "install-deps";
  # when the shell starts, we can run these commands to copy over our dependencies.
  shellHook = pp2n-utils.mkDefaultShellHook packages packageDrvs;
}
