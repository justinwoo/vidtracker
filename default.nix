{ pkgs ? import <nixpkgs> {} }:

let
  easy-ps = import ./easy-ps.nix { inherit pkgs; };

in pkgs.stdenv.mkDerivation {
  name = "easy-purescript";
  buildInputs = [
    easy-ps.purs
    easy-ps.spago
    easy-ps.zephyr
    easy-ps.psc-package2nix
  ];
}
