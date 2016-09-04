{ stdenv, cmake, fetchFromGitHub }:

stdenv.mkDerivation rec {
  name = "libmsgpack-${version}";
  version = "2.0.0";

  src = fetchFromGitHub {
    owner = "msgpack";
    repo = "msgpack-c";
    rev = "cpp-${version}";
    sha256 = "189m44pwpcpf7g4yhzfla4djqyp2kl54wxmwfaj94gwgj5s370i7";
  };

  nativeBuildInputs = [ cmake ];

  crossAttrs = {
  } // stdenv.lib.optionalAttrs (stdenv.cross.libc == "msvcrt") {
    cmakeFlags = "-DMSGPACK_BUILD_EXAMPLES=OFF -DCMAKE_SYSTEM_NAME=Windows";
  };

  meta = with stdenv.lib; {
    description = "MessagePack implementation for C and C++";
    homepage = http://msgpack.org;
    maintainers = with maintainers; [ redbaron wkennington ];
    license = licenses.asl20;
    platforms = platforms.all;
  };
}
