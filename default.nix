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
pkgs.haskellPackages.buildLocalCabalWithArgs {
  inherit src;
  name = "kraken";
  args = { };
}
