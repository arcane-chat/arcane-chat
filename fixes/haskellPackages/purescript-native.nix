{ mkDerivation, aeson, aeson-better-errors, aeson-pretty
, ansi-terminal, ansi-wl-pprint, base, base-compat, bower-json
, boxes, bytestring, clock, containers, data-ordlist, directory
, dlist, edit-distance, fetchgit, file-embed, filepath, foldl
, fsnotify, Glob, haskeline, hspec, hspec-discover, http-client
, http-types, HUnit, language-javascript, lens, lifted-base
, monad-control, monad-logger, mtl, network, optparse-applicative
, parallel, parsec, pattern-arrows, pipes, pipes-http, process
, protolude, regex-tdfa, safe, semigroups, silently, sourcemap
, spdx, split, stdenv, stm, syb, system-filepath, text, time
, transformers, transformers-base, transformers-compat, turtle
, unordered-containers, utf8-string, vector, wai, wai-websockets
, warp, websockets
}:
mkDerivation {
  pname = "purescript";
  version = "0.10.3";
  src = fetchgit {
    url = "https://github.com/andyarvanitis/purescript-native.git";
    sha256 = "1hzfm44qaw856f6fg73ggm4xpv902njimbdhqjap2zsr54ghj5cy";
    rev = "da472aa444c397ddc91402e0c0fbf58404da542b";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-better-errors ansi-terminal base base-compat bower-json
    boxes bytestring clock containers data-ordlist directory dlist
    edit-distance filepath fsnotify Glob haskeline http-client
    http-types language-javascript lens lifted-base monad-control
    monad-logger mtl parallel parsec pattern-arrows pipes pipes-http
    process protolude regex-tdfa safe semigroups sourcemap spdx split
    stm syb text time transformers transformers-base
    transformers-compat unordered-containers utf8-string vector
  ];
  executableHaskellDepends = [
    aeson aeson-pretty ansi-terminal ansi-wl-pprint base base-compat
    boxes bytestring containers directory file-embed filepath foldl
    Glob haskeline http-types monad-logger mtl network
    optparse-applicative parsec process protolude split stm
    system-filepath text time transformers transformers-compat turtle
    utf8-string wai wai-websockets warp websockets
  ];
  testHaskellDepends = [
    aeson aeson-better-errors base base-compat boxes bytestring
    containers directory filepath Glob haskeline hspec hspec-discover
    HUnit mtl optparse-applicative parsec process protolude silently
    stm text time transformers transformers-compat utf8-string vector
  ];
  homepage = "http://www.purescript.org/";
  description = "PureScript Programming Language Compiler";
  license = stdenv.lib.licenses.bsd3;
}
