{ stdenv, chat-shaker, qtbase, pkgconfig }:

stdenv.mkDerivation {
  name = "arcane-chat";
  src = ./chat;
  nativeBuildInputs = [ chat-shaker pkgconfig ];
  buildInputs = [ qtbase ];
  buildPhase = ''
    pwd
    env | grep -i --color=always buildInputs
    chat-shaker
    ls -ltrh
  '';
}