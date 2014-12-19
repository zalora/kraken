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
  name = "kraken-web";
  cabalDrvArgs = {
     buildTools = [ pkgs.graphviz pkgs.file ];
  };
  args = {
     kraken = import ../kraken {};
     kraken-daemon = import ../kraken-daemon {};
  };
}
