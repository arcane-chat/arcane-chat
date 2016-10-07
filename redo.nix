{ stdenv, chat-shaker, qtbase, pkgconfig, protobuf3_0, strace, libtoxcore-dev,
  glib, glibmm, gst_all_1, libsigcxx, qtscript, libsodium, sqlite }:

stdenv.mkDerivation {
  name = "arcane-chat-not-stired";
  src = ./chat;
  nativeBuildInputs = [
    chat-shaker pkgconfig protobuf3_0 strace
  ];
  buildInputs = with gst_all_1; [
    qtbase qtscript
    gstreamer gstreamermm gst-plugins-base
    qt-gstreamer
    libtoxcore-dev libsodium sqlite
    glib glibmm protobuf3_0
  ];
  inherit glibmm libsigcxx;
  glibmmdev = glibmm.dev;
  gstreamermmdev = gst_all_1.gstreamermm.dev;
  gstreamermm = gst_all_1.gstreamermm.out;

  crossAttrs = {
    glibmmdev = glibmm.crossDrv.dev;
    gstreamermmdev = gst_all_1.gstreamermm.crossDrv.dev;
    gstreamermm = gst_all_1.gstreamermm.crossDrv.out;
  };

  postUnpack = ''
    unset autoreconfPhase
    function autoreconfPhase() {
      echo not reconf
    }
  '';

  buildPhase = ''
    chat-shaker --timings -w -j7 -k
  '';
  installPhase = ''
    mkdir -pv $out/bin
    chat-shaker install
  '';
}
