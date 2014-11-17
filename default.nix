let
  filterHaskellSource = builtins.filterSource (path: type:
    type != "unknown" &&
    baseNameOf path != ".git" &&
    baseNameOf path != "result" &&
    baseNameOf path != "dist");
in
{ pkgs ? import <nixpkgs> { }
, src ? filterHaskellSource ./.
}:
let
  servantRepo = ./servant;
  servant-new-impl = pkgs.haskellPackages.buildLocalCabalWithArgs {
    name = "servant";
    src = "${servantRepo}/servant/";
  };
in
pkgs.haskellPackages.buildLocalCabalWithArgs {
  inherit src;
  name = "kraken";
  cabalDrvArgs = {
    # The tests don't work, because they expect the 'dot'
    # executable in the PATH.
    doCheck = false;
  };
  args = {
    servant = servant-new-impl;
  };
}
