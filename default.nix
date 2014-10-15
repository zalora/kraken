{ pkgs ? import <nixpkgs> { }
, src ?  builtins.filterSource (path: type:
    type != "unknown" &&
    baseNameOf path != ".git" &&
    baseNameOf path != "result" &&
    baseNameOf path != "dist") ./.
}:
pkgs.haskellPackages.buildLocalCabalWithArgs {
  inherit src;
  name = "kraken";
  cabalDrvArgs = {
    # The tests don't work, because they expect the 'dot'
    # executable in the PATH.
    doCheck = false;
  };
}
