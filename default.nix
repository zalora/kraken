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
  useLocalServant = false;
  servantSrc = if useLocalServant
    then filterHaskellSource <servant>
    else pkgs.fetchgit {
      url = "https://github.com/alpmestan/servant.git";
      rev = "76ccb07e8d37f643a9abb1f14c57975af8f7d3c9";
      sha256 = "fdf6d21b9c1fb81c4c135d37481d52b38a8162de74d1f7ffe2fc92e75ad65270";
     };
  servant-new-impl = pkgs.haskellPackages.buildLocalCabalWithArgs {
    name = "servant";
    src = "${servantSrc}/servant/";
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
