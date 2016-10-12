{ stdenv, fetchurl, pkgconfig, file, glibmm, gst-plugins-base }:

with stdenv.lib;

let nativeLinux = stdenv.isLinux && !(stdenv ? cross);
in stdenv.mkDerivation rec {
  name = "gstreamermm-1.8.0";
  src = fetchurl {
    url    = "mirror://gnome/sources/gstreamermm/1.8/${name}.tar.xz";
    sha256 = "0i4sk6ns4dyi4szk45bkm4kvl57l52lgm15p2wg2rhx2gr2w3qry";
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

  patches = [ ./iface.patch ];

  meta = with stdenv.lib; {
    description = "C++ interface for GStreamer";
    homepage = http://gstreamer.freedesktop.org/bindings/cplusplus.html;
    license = licenses.lgpl21Plus;
    maintainers = with maintainers; [ romildo ];
    platforms = platforms.unix;
  };
}
