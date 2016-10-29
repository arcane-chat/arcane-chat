{ stdenv, fetchurl, pkgconfig, swig, sphinxbase, gst_all_1 }:

stdenv.mkDerivation rec {
  name = "pocketsphinx-5prealpha";

  src = fetchurl {
    url = "mirror://sourceforge/cmusphinx/${name}.tar.gz";
    sha256 = "1n9yazzdgvpqgnfzsbl96ch9cirayh74jmpjf7svs4i7grabanzg";
  };

  patches = [ ./fix-caps.patch ];

  configureFlags = [ "--without-python" ];

  nativeBuildInputs = [ pkgconfig swig ];

  buildInputs = with gst_all_1; [ gstreamer gst-plugins-base ];

  propagatedBuildInputs = [ sphinxbase ];

  meta = with stdenv.lib; {
    description = "Voice recognition library written in C";
    homepage    = http://cmusphinx.sourceforge.net;
    license     = licenses.free;
    platforms   = platforms.linux;
  };
}
