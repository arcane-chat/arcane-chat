{ stdenv, fetchFromGitHub, pkgconfig, autoconf, automake, libtool, unzip, file
, mm-common, glibmm, gtkmm, gst_all_1 }:

# let
#   ver_maj = "1.4";
#   ver_min = "3";
# in
stdenv.mkDerivation rec {
  # name = "gstreamermm-${ver_maj}.${ver_min}";
  # src = fetchurl {
  #   url    = "mirror://gnome/sources/gstreamermm/${ver_maj}/${name}.tar.xz";
  #   sha256 = "0bj6and9b26d32bq90l8nx5wqh2ikkh8dm7qwxyxfdvmrzhixhgi";
  # };

  name = "gstreamermm-20160828";

  src = fetchFromGitHub {
    repo = "gstreamermm";
    owner = "GNOME";
    rev = "6dbbbb3cd6133392f64140627d2e3f2e5e36d60d";
    sha256 = "03ra1wgcy7szb5gmhzz5mwb4cam78xc3b4vnc1nbi0abm7yq367j";
  };

  outputs = [ "dev" "out" ];

  nativeBuildInputs = [ pkgconfig autoconf automake libtool mm-common unzip file ];

  propagatedBuildInputs = [ glibmm gtkmm gst_all_1.gst-plugins-base ];

  enableParallelBuilding = true;

  configureFlagsArray = ["--disable-unittests"];
  
  preConfigure = ''
      NOCONFIGURE=1 ./autogen.sh
  '';

  meta = with stdenv.lib; {
    description = "C++ interface for GStreamer";
    homepage = http://gstreamer.freedesktop.org/bindings/cplusplus.html;
    license = licenses.lgpl21Plus;
    maintainers = with maintainers; [ romildo ];
    platforms = platforms.unix;
  };

}
