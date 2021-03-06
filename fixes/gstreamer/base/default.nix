{ stdenv, fetchurl, pkgconfig, python, gstreamer, gobjectIntrospection
, orc, alsaLib, libXv, pango, libtheora, libopus
, cdparanoia, libvisual, libintlOrEmpty
}:

stdenv.mkDerivation rec {
  name = "gst-plugins-base-1.9.2";

  meta = {
    description = "Base plugins and helper libraries";
    homepage = "http://gstreamer.freedesktop.org";
    license = stdenv.lib.licenses.lgpl2Plus;
    platforms = stdenv.lib.platforms.unix;
  };

  src = fetchurl {
    url = "${meta.homepage}/src/gst-plugins-base/${name}.tar.xz";
    sha256 = "1phbqpw0ackn8pdiwwl04pq7vdck7ls755z03lcw80fkl5jaj95i";
  };

  outputs = [ "dev" "out" ];

  nativeBuildInputs = [
    pkgconfig python gobjectIntrospection
  ];

  buildInputs = [
    orc libtheora libopus
  ]
  ++ libintlOrEmpty
  ++ stdenv.lib.optionals (stdenv.isLinux && !(stdenv ? cross)) [
    alsaLib
    cdparanoia
    pango
    libvisual
    libXv
  ];

  propagatedBuildInputs = [ gstreamer ];

  configureFlags = if (!stdenv.isLinux || (stdenv ? cross)) then [
    "--disable-shared"
    "--enable-static"
    "--disable-cdparanoia"
    "--disable-libvisual"
    "--disable-pango"
    "--disable-x"
    "--disable-fatal-warnings"
  ] else [
  ];

  NIX_LDFLAGS = if stdenv.isDarwin then "-lintl" else null;

  enableParallelBuilding = true;
}

