{ stdenv, cmake, doxygen }:

stdenv.mkDerivation {
  name = "arcane-chat-doc";
  src = ./chat;
  nativeBuildInputs = [ cmake doxygen ];
  postUnpack = ''
    pwd
    ls -ltrh
    export sourceRoot=chat/doc
  '';
  cmakeFlags = [ "-DBUILD_DOCUMENTATION=1" ];
  postInstall = ''
    mkdir $out/nix-support
    echo "doc manual $out/share/doc/arcane-chat index.html" \
        >> $out/nix-support/hydra-build-products
'';
}