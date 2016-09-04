{ nixpkgs ? { outPath = <nixpkgs>; } }:

let default = import ./default.nix { inherit nixpkgs; };
in {
  fuspr-chat.linux = default.linux.fuspr-chat;
  # windows = default.windows.fuspr-chat;
  # darwin = default.darwin.fuspr-chat;
}
