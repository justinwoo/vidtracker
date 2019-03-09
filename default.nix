{ pkgs ? import <nixpkgs> {} }:

let
  easy-ps = import ./easy-ps.nix { inherit pkgs; };

in pkgs.stdenv.mkDerivation {
  name = "easy-purescript";
  buildInputs = [
    easy-ps.purs
    easy-ps.psc-package2nix
  ];
}
