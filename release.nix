{ nixpkgs ? { outPath = <nixpkgs>; } }:

let
  default = import ./default.nix { inherit nixpkgs; };
  lib = default.linux.super.lib;
  makeJob = x: {
    ${x} = {
      windows = (lib.attrByPath (lib.splitString "." x) null default.windows).crossDrv;
      linux = lib.attrByPath (lib.splitString "." x) null default.linux;
    };
  };
  makeJobs = lib.foldl (total: next: total // (makeJob next)) {};
in {
  wineWow-linux = default.linux.super.wine.override { wineBuild = "wineWow"; };
  chat-shaker = default.linux.super.chat-shaker;
  # darwin = default.darwin.arcane-chat;
} // makeJobs [
  "arcane-chat"
  "super.arcane-chat"
  "super.gst_all_1.gstreamermm"
  "super.gst_all_1.gst-plugins-good"
  "super.qt56.qtbase"
  "super.qt56.qtdeclarative"
  "super.x264"
  ]
