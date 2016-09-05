{ stdenv, fetchurl, pkgconfig, python, yasm, gst-plugins-base, orc, bzip2 }:

stdenv.mkDerivation rec {
  name = "gst-libav-1.9.2";

  meta = {
    homepage = "http://gstreamer.freedesktop.org";
    license = stdenv.lib.licenses.lgpl2Plus;
    platforms = stdenv.lib.platforms.linux;
  };

  src = fetchurl {
    url = "${meta.homepage}/src/gst-libav/${name}.tar.xz";
    sha256 = "0assg80570ff2pky3mcr44b01zxgj7gpcm28kmvxg1ng1d91593q";
  };

  outputs = [ "dev" "out" ];

  nativeBuildInputs = with stdenv.lib; [ pkgconfig python yasm ];

  buildInputs = with stdenv.lib; [ gst-plugins-base orc bzip2 ];
}
