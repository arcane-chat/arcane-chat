{ nixpkgs ? { outPath = <nixpkgs>; } }:

let default = import ./default.nix { inherit nixpkgs; };
in {
  arcane-chat.linux = default.linux.arcane-chat;
  wineWow-linux = default.linux.super.wine.override { wineBuild = "wineWow"; };
  arcane-chat-windows = default.windows.arcane-chat.crossDrv;
  # darwin = default.darwin.arcane-chat;
}
