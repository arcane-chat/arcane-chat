{ stdenv, fetchFromGitHub, pkgconfig, autoconf, automake, file }:

stdenv.mkDerivation rec {
  name = "mm-common-20160719";

  src = fetchFromGitHub {
    repo = "mm-common";
    owner = "GNOME";
    rev = "71a4333c8fbc85047a315dc126fe9e8e4619a6b6";
    sha256 = "03ahjqb2b5n74b4mqfpmq8qc7n26ky936q94agdf2l84k7qsagr3";
  };

  nativeBuildInputs = [ pkgconfig autoconf automake file ];

  configurePhase = ''
      cp ${./libstdc++.tag} "doctags/libstdc++.tag"
      ./autogen.sh --disable-network --prefix=$out
  '';
}
