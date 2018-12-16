{ pkgs ? import <nixpkgs> {} }:

let
  easy-ps = import ./easy-ps.nix { inherit pkgs; };

  psc-package2nix = import ./psc-package2nix.nix { inherit pkgs; };

in pkgs.stdenv.mkDerivation {
  name = "easy-purescript";
  buildInputs = easy-ps.buildInputs;
}
