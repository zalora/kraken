{ pkgs ? import <nixpkgs> { }
, src ?  builtins.filterSource (path: type:
    type != "unknown" &&
    baseNameOf path != ".git" &&
    baseNameOf path != "result" &&
    baseNameOf path != "dist") ./.
}:
let
  # Force old QC for these
  needsOldQC = [
    "hspec"
    "quickcheckIo"
    "quickcheckInstances"
    "enclosedExceptions"
    "interpolate"
  ];

  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: builtins.listToAttrs (map (name: {
      inherit name;
      value = super.${name}.override {
        QuickCheck = self.QuickCheck_2_6;
      };
    }) needsOldQC);
  };
in

(haskellPackages.buildLocalCabal src "kraken").override {
  QuickCheck = haskellPackages.QuickCheck_2_6;
}
