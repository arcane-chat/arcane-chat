{ stdenv, fetchFromGitHub, pkgconfig, qtbase, protobuf3_0 }:

stdenv.mkDerivation rec {
  name = "protoc-gen-doc-20160730";

  src = fetchFromGitHub {
    owner  = "estan";
    repo   = "protoc-gen-doc";
    rev    = "225664a903cebe0823669bdc5ea97ea9cbd80989";
    sha256 = "0xjzywxcih5vzi5g3qd46vqah4ybc59hb74rm5b9f40q1djhxixl";
  };

  buildInputs = [ pkgconfig qtbase protobuf3_0 ];

  postConfigure = ''
      qmake
      export INSTALL_ROOT=$out
  '';

  patches = [ ./usr-local.patch ];
}
