{ stdenv, lib, fetchgit, copyPathsToStore
, srcs

, xlibs, libX11, libxcb, libXcursor, libXext, libXrender, libXi
, xcbutil, xcbutilimage, xcbutilkeysyms, xcbutilwm, libxkbcommon
, fontconfig, freetype, harfbuzz
, openssl, dbus, glib, udev, libxml2, libxslt, pcre16
, zlib, libjpeg, libpng, libtiff, sqlite, icu, ed

, coreutils, bison, flex, gdb, gperf, lndir, ruby
, patchelf, perl, pkgconfig, python

# optional dependencies
, cups ? null
, mysql ? null, postgresql ? null

# options
, mesaSupported, mesa
, buildExamples ? false
, buildTests ? false
, developerBuild ? false
, decryptSslTraffic ? false
}:

let
  system-x86_64 = lib.elem stdenv.system lib.platforms.x86_64;
in

stdenv.mkDerivation rec {

  name = "qtbase-${srcs.qtbase.version}";
  inherit (srcs.qtbase) src version;

  outputs = [ "out" "dev" ];

  patches =
    copyPathsToStore (lib.readPathsFromFile ./. ./series)
    ++ lib.optional decryptSslTraffic ./decrypt-ssl-traffic.patch
    ++ lib.optional mesaSupported [ ./dlopen-gl.patch ./mkspecs-libgl.patch ];

  postPatch =
    ''
      substituteInPlace configure --replace /bin/pwd pwd
      substituteInPlace src/corelib/global/global.pri --replace /bin/ls ${coreutils}/bin/ls
      sed -e 's@/\(usr\|opt\)/@/var/empty/@g' -i config.tests/*/*.test -i mkspecs/*/*.conf

      sed -i 's/PATHS.*NO_DEFAULT_PATH//' "src/corelib/Qt5Config.cmake.in"
      sed -i 's/PATHS.*NO_DEFAULT_PATH//' "src/corelib/Qt5CoreMacros.cmake"
      sed -i 's/NO_DEFAULT_PATH//' "src/gui/Qt5GuiConfigExtras.cmake.in"
      sed -i 's/PATHS.*NO_DEFAULT_PATH//' "mkspecs/features/data/cmake/Qt5BasicConfig.cmake.in"

      substituteInPlace src/network/kernel/qdnslookup_unix.cpp \
        --replace "@glibc@" "${stdenv.cc.libc.out}"
      substituteInPlace src/network/kernel/qhostinfo_unix.cpp \
        --replace "@glibc@" "${stdenv.cc.libc.out}"

      substituteInPlace src/plugins/platforms/xcb/qxcbcursor.cpp \
        --replace "@libXcursor@" "${libXcursor.out}"

      substituteInPlace src/network/ssl/qsslsocket_openssl_symbols.cpp \
        --replace "@openssl@" "${openssl.out}"

      substituteInPlace src/dbus/qdbus_symbols.cpp \
        --replace "@dbus_libs@" "${dbus.lib}"

      substituteInPlace \
        src/plugins/platforminputcontexts/compose/generator/qtablegenerator.cpp \
        --replace "@libX11@" "${libX11.out}"
    ''
    + lib.optionalString mesaSupported ''
      substituteInPlace \
        src/plugins/platforms/xcb/gl_integrations/xcb_glx/qglxintegration.cpp \
        --replace "@mesa_lib@" "${mesa.out}"
      substituteInPlace mkspecs/common/linux.conf \
        --replace "@mesa_lib@" "${mesa.out}" \
        --replace "@mesa_inc@" "${mesa.dev or mesa}"
    '';


  setOutputFlags = false;
  preConfigure = ''
    export LD_LIBRARY_PATH="$PWD/lib:$PWD/plugins/platforms:$LD_LIBRARY_PATH"
    export MAKEFLAGS=-j$NIX_BUILD_CORES

    configureFlags+="\
        -plugindir $out/lib/qt5/plugins \
        -importdir $out/lib/qt5/imports \
        -qmldir $out/lib/qt5/qml \
        -docdir $out/share/doc/qt5"
  '';

  prefixKey = "-prefix ";

  # -no-eglfs, -no-directfb, -no-linuxfb and -no-kms because of the current minimalist mesa
  # TODO Remove obsolete and useless flags once the build will be totally mastered
  configureFlags = ''
    -verbose
    -confirm-license
    -opensource

    -shared
    -c++11
    ${lib.optionalString developerBuild "-developer-build"}
    -largefile
    -accessibility
    -rpath
    -optimized-qmake
    -strip
    -no-reduce-relocations
    -system-proxies
    -pkg-config

    -gui
    -widgets
    -opengl desktop
    -qml-debug
    -iconv
    -icu
    -pch
    -glib
    -${lib.optionalString (cups == null) "no-"}cups

    -no-eglfs
    -no-directfb
    -no-linuxfb
    -no-kms

    ${lib.optionalString (!system-x86_64) "-no-sse2"}
    -no-sse3
    -no-ssse3
    -no-sse4.1
    -no-sse4.2
    -no-avx
    -no-avx2
    -no-mips_dsp
    -no-mips_dspr2

    -system-libpng
    -system-libjpeg
    -system-xkbcommon
    -system-pcre
    -openssl-linked
    -no-dbus

    -system-sqlite
    -${if mysql != null then "plugin" else "no"}-sql-mysql
    -${if postgresql != null then "plugin" else "no"}-sql-psql

    -make libs
    -make tools
    -${lib.optionalString (buildExamples == false) "no"}make examples
    -${lib.optionalString (buildTests == false) "no"}make tests
  '';

  # PostgreSQL autodetection fails sporadically because Qt omits the "-lpq" flag
  # if dependency paths contain the string "pq", which can occur in the hash.
  # To prevent these failures, we need to override PostgreSQL detection.
  PSQL_LIBS = lib.optionalString (postgresql != null) "-L${postgresql.lib}/lib -lpq";

  propagatedBuildInputs = [
    dbus glib libxml2 libxslt openssl pcre16 sqlite udev zlib

    # Image formats
    libjpeg libpng libtiff

    # Text rendering
    fontconfig freetype harfbuzz icu

    # X11 libs
    xlibs.libXcomposite libX11 libxcb libXext libXrender libXi
    xcbutil xcbutilimage xcbutilkeysyms xcbutilwm libxkbcommon
  ]
  ++ lib.optional mesaSupported mesa;

  buildInputs =
    [ bison flex gperf ruby ]
    ++ lib.optional developerBuild gdb
    ++ lib.optional (cups != null) cups
    ++ lib.optional (mysql != null) mysql.lib
    ++ lib.optional (postgresql != null) postgresql;

  nativeBuildInputs = [ lndir patchelf perl pkgconfig python ed ];

  # freetype-2.5.4 changed signedness of some struct fields
  NIX_CFLAGS_COMPILE = "-Wno-error=sign-compare";

  postInstall = ''
    find "$out" -name "*.cmake" | while read file; do
        substituteInPlace "$file" \
            --subst-var-by NIX_OUT "$out" \
            --subst-var-by NIX_DEV "$dev"
    done
  '';

  preFixup = ''
    # We cannot simply set these paths in configureFlags because libQtCore retains
    # references to the paths it was built with.
    moveToOutput "bin" "$dev"
    moveToOutput "include" "$dev"
    moveToOutput "mkspecs" "$dev"

    # The destination directory must exist or moveToOutput will do nothing
    mkdir -p "$dev/share"
    moveToOutput "share/doc" "$dev"
  '';

  postFixup =
    ''
      # Don't retain build-time dependencies like gdb and ruby.
      sed '/QMAKE_DEFAULT_.*DIRS/ d' -i $dev/mkspecs/qconfig.pri

      # Move libtool archives and qmake projects
      if [ "z''${!outputLib}" != "z''${!outputDev}" ]; then
          pushd "''${!outputLib}"
          find lib -name '*.a' -o -name '*.la' -o -name '*.prl' | \
              while read -r file; do
                  mkdir -p "''${!outputDev}/$(dirname "$file")"
                  mv "''${!outputLib}/$file" "''${!outputDev}/$file"
              done
          popd
      fi
    '';

  inherit lndir;
  setupHook = ./setup-hook.sh;

  enableParallelBuilding = true;

  crossAttrs = {
    configureFlags = [ configureFlags ] ++ [''
      -xplatform win32-g++
      -qpa windows
      -qt-zlib
    ''];
    preConfigure = ''
      crossPrefix=$(type -p $CXX | sed s/g++//)
      export configureFlags="$configureFlags -device-option CROSS_COMPILE=''${crossConfig}-"

      #sed -i "s/\\\''$\\\''$[{]CROSS_COMPILE[}]gcc/$CC/" mkspecs/win32-g++/qmake.conf
      #sed -i "s/\\\''$\\\''$[{]CROSS_COMPILE[}]g++/$CXX/" mkspecs/win32-g++/qmake.conf
      export NIX_CROSS_LDFLAGS="-lws2_32 -lwinmm -lole32 $NIX_CROSS_LDFLAGS";
      unset CXX
      unset CC
      ed -s src/corelib/corelib.pro << EOF
      0a
      message("corelib.pro \''$\''$QMAKE_CC \''$\''$QMAKE_CXX")
      .
      w
      EOF
      head -n1 src/corelib/corelib.pro
      #mv -vi mkspecs/win32-g++/qmake.conf qmake.temp
      #cat qmake.temp | egrep -v "QMAKE_CC |QMAKE_CXX " > mkspecs/win32-g++/qmake.conf
      #rm qmake.temp
      #cat mkspecs/win32-g++/qmake.conf
      #exit 1
    '';
    #NIX_DEBUG = true;
    dontSetConfigureCross=true;
  };

  meta = with lib; {
    homepage = http://www.qt.io;
    description = "A cross-platform application framework for C++";
    license = with licenses; [ fdl13 gpl2 lgpl21 lgpl3 ];
    maintainers = with maintainers; [ bbenoist qknight ttuegel ];
    platforms = platforms.linux;
  };

}
