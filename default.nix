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
      rev = "0129dea9b6d20fb866121a1e6131e42d6122cba1";
      sha256 = "cf9bc6ce82a14c03073064fee1898521e860c41821514ebaf4fb5a8354c80fb4";
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
