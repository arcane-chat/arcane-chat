{ stdenv, fetchurl, fltk, openssl, libpng, libjpeg }:

stdenv.mkDerivation rec {
  name = "htmldoc-1.8.29";

  src = fetchurl {
    url = "http://www.msweet.org/files/project1/${name}-source.tar.bz2";
    sha256 = "0c9zhni93lbag0c9vacvlnpw95f7w4p8wp1hp3m6k4fi83bnmjg8";
  };

  buildInputs = [ fltk openssl libpng libjpeg ];

  meta = {
    homepage = http://www.htmldoc.org/;
    description = "Converts HTML files to indexed HTML, PS or PDF";
    license = stdenv.lib.licenses.gpl2;
    maintainers = with stdenv.lib.maintainers; [ viric ];
    platforms = with stdenv.lib.platforms; linux;
  };
}
