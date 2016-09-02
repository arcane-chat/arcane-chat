{ pkgs ? import <nixpkgs> {} }:

let
  callPackage = pkgs.newScope self;
  self = rec {
    fuspr-chat = callPackage ./chat {};
  };
in self
