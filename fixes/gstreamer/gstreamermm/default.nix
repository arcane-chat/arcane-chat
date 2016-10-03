{ stdenv, fetchurl, pkgconfig, file, glibmm, gst-plugins-base }:

with stdenv.lib;

let nativeLinux = stdenv.isLinux && !(stdenv ? cross);
in stdenv.mkDerivation rec {
  name = "gstreamermm-1.4.3";
  src = fetchurl {
    url    = "mirror://gnome/sources/gstreamermm/1.4/${name}.tar.xz";
    sha256 = "0bj6and9b26d32bq90l8nx5wqh2ikkh8dm7qwxyxfdvmrzhixhgi";
  };

  outputs = [ "dev" "out" ];

  nativeBuildInputs = [ pkgconfig file ];

  propagatedBuildInputs = [ glibmm gst-plugins-base ];

  enableParallelBuilding = true;

  configureFlags = optionals (!nativeLinux) [
    "--disable-shared"
    "--enable-static"
    "--disable-unittests"
  ];

  patches = [
    ./glibmm-2.50.patch
    ./iface.patch
  ];

  meta = with stdenv.lib; {
    description = "C++ interface for GStreamer";
    homepage = http://gstreamer.freedesktop.org/bindings/cplusplus.html;
    license = licenses.lgpl21Plus;
    maintainers = with maintainers; [ romildo ];
    platforms = platforms.unix;
  };
}
