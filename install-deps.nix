let
  pkgs = import <nixpkgs> {};

  # import our packages
  packages = import ./packages.nix {};

  # these are the package derivations we will want to work with
  packageDrvs = builtins.attrValues packages.inputs;

  # these are the copy commands for copying our dependencies from the nix store
  # this means that if we have overlapping packages between sets or between projects,
  # they come from the same source!
  copyCmds = map (x: let target = ".psc-package/${packages.set}/${x.name}/${x.version}"; in ''
    mkdir -p ${target}
    echo ${target}
    echo ${toString x.outPath}
    cp --no-preserve=mode,ownership,timestamp -r ${toString x.outPath}/* ${target}
  '') packageDrvs;

in pkgs.stdenv.mkDerivation {
  name = "install-deps";
  src = ./.;

  # make sure we depend on these explicitly
  buildInputs = packageDrvs;

  # when the shell starts, we can run these commands to copy over our dependencies.
  shellHook = toString copyCmds;
}
