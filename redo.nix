{ stdenv, chat-shaker, qtbase, pkgconfig, protobuf3_0, strace, libtoxcore-dev,
  glib, glibmm, gst_all_1, libsigcxx, qtscript, libsodium, sqlite } @ args:

stdenv.mkDerivation rec {
  name = "arcane-chat-not-stirred";
  src = ./chat;
  nativeBuildInputs = [
    chat-shaker pkgconfig protobuf3_0 strace
  ];
  buildInputs = with gst_all_1; [
    qtbase qtscript
    gstreamer gst_all_1.gstreamermm gst-plugins-base
    qt-gstreamer
    libtoxcore-dev libsodium sqlite
    glib glibmm protobuf3_0
  ];
  inherit glibmm libsigcxx;
  glibmmdev = glibmm.dev;
  gstreamermmdev = gst_all_1.gstreamermm.dev;
  gstreamermm = gst_all_1.gstreamermm.out;
  shakeArgs = [ "--timings" "--debug-build" ];

  crossAttrs = {
    glibmmdev = args.glibmm.crossDrv.dev;
    gstreamermmdev = gst_all_1.gstreamermm.crossDrv.dev;
    gstreamermm = gst_all_1.gstreamermm.crossDrv.out;
    shakeArgs = shakeArgs ++ [ "--windows" ];
    # todo: store more log details to $out that a user could download
    succeedOnFailure = true;
  };

  postUnpack = ''
    unset autoreconfPhase
    function autoreconfPhase() {
      echo not reconf
      set -x
    }
    unset _linkDLLs
    source ${./fixes/win-dll-link.sh}
  '';

  buildPhase = ''
    mkdir $out
    chat-shaker ''${enableParallelBuilding:+-j''${NIX_BUILD_CORES}} $shakeArgs
  '';

  enableParallelBuilding = true;

  installPhase = ''
    mkdir -pv $out/bin $out/nix-support $out/shake
    chat-shaker install $shakeArgs
    echo "doc buildReport $out/shake report.html" \
    >> $out/nix-support/hydra-build-products
  '';
}
