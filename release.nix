{ nixpkgs ? { outPath = <nixpkgs>; } }:

let default = import ./default.nix { inherit nixpkgs; };
in {
  arcane-chat.linux = default.linux.arcane-chat;
  # windows = default.windows.arcane-chat;
  # darwin = default.darwin.arcane-chat;
}
