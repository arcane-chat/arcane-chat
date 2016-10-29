{ stdenv, fetchurl, pkgconfig, bison, swig }:

stdenv.mkDerivation rec {
  name = "sphinxbase-5prealpha";

  src = fetchurl {
    url = "mirror://sourceforge/cmusphinx/${name}.tar.gz";
    sha256 = "0vr4k8pv5a8nvq9yja7kl13b5lh0f9vha8fc8znqnm8bwmcxnazp";
  };

  nativeBuildInputs = [ pkgconfig bison swig ];

  configureFlags = [
    "--without-python"
  ];

  meta = with stdenv.lib; {
    description = "Support library for pocketsphinx";
    homepage    = "http://cmusphinx.sourceforge.net";
    license     = licenses.bsd2;
    platforms   = platforms.all;
    maintainers = with maintainers; [ fuuzetsu ];
  };
}
