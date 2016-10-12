{ stdenv, chat-shaker, pkgconfig, qtbase, qtscript, libtoxcore-dev
, libsodium, sqlite, openssl, glib, glibmm, gst_all_1, libsigcxx
, protobuf3_0, zeromq4, cppzmq
} @ args:

stdenv.mkDerivation rec {
  name = "arcane-chat-not-stirred";
  src = ./chat;

  outputs = [ "out" "report" ];

  nativeBuildInputs = [
    chat-shaker pkgconfig protobuf3_0
  ];
  buildInputs = with gst_all_1; [
    qtbase qtscript
    glib glibmm gstreamer gst_all_1.gstreamermm gst-plugins-base qt-gstreamer
    zeromq4 cppzmq protobuf3_0 libtoxcore-dev libsodium sqlite
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
    postInstall = ''
      ln -sv ${qtbase.crossDrv.dev}/bin/Qt5Gui.dll $out/bin
      ln -sv ${qtbase.crossDrv.dev}/bin/Qt5Core.dll $out/bin
      ln -sv ${qtbase.crossDrv.dev}/bin/Qt5Network.dll $out/bin
      ln -sv ${qtbase.crossDrv.dev}/bin/Qt5Widgets.dll $out/bin
      ln -sv ${qtscript.crossDrv.dev}/bin/Qt5Script.dll $out/bin
      ln -sv ${openssl.crossDrv.bin}/bin/libeay32.dll $out/bin/LIBEAY32.dll
      ln -sv ${openssl.crossDrv.bin}/bin/ssleay32.dll $out/bin/SSLEAY32.dll
    '';
    dontCrossStrip = true;
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
    runHook preInstall
    mkdir -pv $out/bin $report/nix-support $report/shake
    chat-shaker install $shakeArgs
    echo "doc buildReport $report/shake report.html" \
    >> $report/nix-support/hydra-build-products
    runHook postInstall
  '';
}
