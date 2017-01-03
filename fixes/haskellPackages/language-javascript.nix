{ mkDerivation, alex, array, base, blaze-builder, bytestring, Cabal
, containers, happy, hspec, mtl, QuickCheck, stdenv, text
, utf8-light, utf8-string
}:
mkDerivation {
  pname = "language-javascript";
  version = "0.6.0.9";
  sha256 = "1k1ji4iia6wpzls5999hirypxsng5bgfrvbv9zjvn9wczga9hsx8";
  libraryHaskellDepends = [
    array base blaze-builder bytestring containers mtl text utf8-string
  ];
  libraryToolDepends = [ alex happy ];
  testHaskellDepends = [
    array base blaze-builder bytestring Cabal containers hspec mtl
    QuickCheck utf8-light utf8-string
  ];
  homepage = "https://github.com/erikd/language-javascript";
  description = "Parser for JavaScript";
  license = stdenv.lib.licenses.bsd3;
}
