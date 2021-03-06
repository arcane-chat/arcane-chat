{ stdenv, fetchurl, fetchpatch, cmake, flex, bison, pkgconfig
, gstreamer, gst-plugins-base, boost, glib, qt56 }:

let self = stdenv.mkDerivation rec {
  name = "${pname}-1.2.0";
  pname = "qt-gstreamer";

  src = fetchurl {
    url = "http://gstreamer.freedesktop.org/src/${pname}/${name}.tar.xz";
    sha256 = "9f3b492b74cad9be918e4c4db96df48dab9c012f2ae5667f438b64a4d92e8fd4";
  };

  patches = [
    (fetchpatch {
      url = "https://cgit.freedesktop.org/gstreamer/qt-gstreamer/patch/?id=e2ca8094aa8d0eac1c3a98df66fe94ce0c754088";
      sha256 = "1qps0nlc26d74wk8h96xl5s3d9qrdx6c0ph0zpl1dnc691lgyf6s";
    })
  ];

  outputs = [ "dev" "out" ];

  buildInputs = [
    gstreamer gst-plugins-base glib qt56.qtbase qt56.qtdeclarative
  ];
  
  propagatedBuildInputs = [ boost ];
  nativeBuildInputs = [ cmake flex bison pkgconfig ];

  cmakeFlags = [
    "-DQT_VERSION=5"
    "-DUSE_QT_PLUGIN_DIR=OFF"
    "-DUSE_GST_PLUGIN_DIR=OFF"
  ];

  crossAttrs = {
    cmakeFlags = cmakeFlags ++ [ "-DCMAKE_SYSTEM_NAME=Windows" ];

    preConfigure = ''
        cp -v ${./CMakeLists.txt} CMakeLists.txt
    '';

    postConfigure = ''
        moc -i ../src/QGlib/connect.cpp -o ../src/connect.moc
    '';
    patches = patches ++ [ ./exports.patch ];
    outputs = [ "dev" "out" ];
  };

  meta = {
    platforms = stdenv.lib.platforms.linux;
  };
};
in self
