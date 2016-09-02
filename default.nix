{ nixpkgs ? import <nixpkgs> }:

let
  crossSystem = {
    config         = "x86_64-w64-mingw32";
    arch           = "x86_64";
    libc           = "msvcrt";
    platform       = {};
    openssl.system = "mingw64";
  };
  pkgs = nixpkgs { inherit crossSystem; };
  #pkgs = nixpkgs {};
  callPackage = pkgs.newScope self;
  self = rec {
    fuspr-chat = callPackage ./chat {};
  };
in self
