{ stdenv, chat-shaker, qtbase, pkgconfig, protobuf3_0, strace, libtoxcore-dev,
  glib, glibmm, gst_all_1 }:

stdenv.mkDerivation {
  name = "arcane-chat-not-stired";
  src = ./chat;
  nativeBuildInputs = [
    chat-shaker pkgconfig protobuf3_0 strace
  ];
  buildInputs = with gst_all_1; [ qtbase gstreamer libtoxcore-dev glib qt-gstreamer glibmm ];
  buildPhase = ''
    pwd
    env | grep -i --color=always buildInputs
    chat-shaker --timings -w -V
    ls -ltrh
  '';
}