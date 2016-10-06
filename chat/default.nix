# Build dependencies
{ stdenv, cmake, pkgconfig, doxygen, ghostscript, ninja, makeWrapper
# Program dependencies
, zeromq4, libmsgpack, libtoxcore-dev, nlohmann_json, obs-studio
, gst_all_1, protobuf3_0, qtbase, qtscript, libsodium
# Misc dependencies
, guile, parallel, buildEnv, glib, glibmm, libsigcxx, enableDebugging
, include-what-you-use, rtags, python
}:

stdenv.mkDerivation rec {
  name = "arcane-chat-0.0.1";

  src = ./.;

  nativeBuildInputs = [
    cmake pkgconfig ninja doxygen ghostscript guile parallel makeWrapper
    protobuf3_0 include-what-you-use rtags python
  ];

  buildInputs = with gst_all_1; [
    zeromq4 libmsgpack nlohmann_json libtoxcore-dev obs-studio
    glib glibmm libsigcxx gstreamermm gstreamer gst-plugins-base
    gst-plugins-good gst-plugins-ugly gst-plugins-bad qt-gstreamer
    protobuf3_0 qtbase qtscript libsodium
  ];
  postUnpack = ''
    unset autoreconfPhase
    function autoreconfPhase() {
      echo not reconf
      env | grep --color=always boost
    }
  '';
  NIX_DEBUG = false;

  cmakeFlags = [ "-GNinja" ];

  buildPhase = "ninja";

  installPhase = ''
      ninja install
      mkdir $out/nix-support
      echo "doc manual $out/share/doc/arcane-chat index.html" \
          >> $out/nix-support/hydra-build-products

      for exe in $out/bin/{client,gst}; do
          wrapProgram "$exe" \
              --prefix GST_PLUGIN_PATH : "$GST_PLUGIN_SYSTEM_PATH_1_0"
      done
  '';
  crossAttrs = {
    cmakeFlags = cmakeFlags ++ [ "-DCMAKE_SYSTEM_NAME=Windows" ];
  };

  shellHook = ''
      cd ${toString src}
      [[ -d "$(pwd)/build" ]] && rm -rf build
      source ${../helpers}
      if [[ -z "$CHAT_NIX_SKIP_CMAKE" ]]; then
          OLD_CMAKE_FLAGS="$cmakeFlags"
          cmakeFlags="-DCMAKE_C_FLAGS=-v -DCMAKE_CXX_FLAGS=-v $cmakeFlags" \
              cmakeConfigurePhase
          BUILD_LOG="$(mktemp -p /tmp build-log.XXXXXX)"
          ninja &> "$BUILD_LOG"
          echo ""
          echo ""
          echo ""
          note "FLYCHECK-CLANG-INCLUDE-PATH:"
          echo ""
          INCLUDE_PATH="$(generate-emacs-include-path "$BUILD_LOG")"
          echo "$INCLUDE_PATH"
          echo ""
          echo ""
          echo ""
          read -p "Copy generated include path into clipboard? [y/N] "
          if text-represents-yes "$REPLY"; then
              echo "$INCLUDE_PATH" | clipboard
          fi
          echo ""
          echo ""
          echo ""
          rm "$BUILD_LOG"
          cmakeFlags="$OLD_CMAKE_FLAGS"
          unset OLD_CMAKE_FLAGS BUILD_LOG
          refresh-plugin-build "${toString src}"
      fi
      note "Entering nix-shell"
  '';
}
