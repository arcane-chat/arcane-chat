{ stdenv, fetchurl, pkgconfig, file, glibmm, gst-plugins-base }:

stdenv.mkDerivation rec {
  name = "gstreamermm-1.4.3";
  src = fetchurl {
    url    = "mirror://gnome/sources/gstreamermm/1.4/${name}.tar.xz";
    sha256 = "0bj6and9b26d32bq90l8nx5wqh2ikkh8dm7qwxyxfdvmrzhixhgi";
  };

  outputs = [ "dev" "out" ];

  nativeBuildInputs = [ pkgconfig file ];

  propagatedBuildInputs = [ glibmm gst-plugins-base ];

  enableParallelBuilding = true;

  configureFlagsArray = ["--disable-unittests"];

  meta = with stdenv.lib; {
    description = "C++ interface for GStreamer";
    homepage = http://gstreamer.freedesktop.org/bindings/cplusplus.html;
    license = licenses.lgpl21Plus;
    maintainers = with maintainers; [ romildo ];
    platforms = platforms.unix;
  };
}
