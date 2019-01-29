#! /usr/bin/env nix-shell
#! nix-shell ./install-deps.nix --run 'exit'

{ pkgs ? import <nixpkgs> {} }:

let
  packages = import ./packages.nix { inherit pkgs; };

  easy-ps = import ./easy-ps.nix { inherit pkgs; };
  pp2n-utils = import (easy-ps.inputs.psc-package2nix.src + "/utils.nix");

in pp2n-utils.mkInstallPackages {
  inherit packages;
}
