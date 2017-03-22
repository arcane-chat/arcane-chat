{ pkgs, haskellPackages }:

with pkgs.haskell.lib;

haskellPackages.override {
  overrides = self: super: {
  };
}
