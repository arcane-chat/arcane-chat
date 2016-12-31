{ pkgs, haskellPackages }:

with pkgs.haskell.lib;

haskellPackages.override {
  overrides = self: super: {
    purescript-native = dontCheck (self.callPackage ./purescript-native.nix {});
  };
}