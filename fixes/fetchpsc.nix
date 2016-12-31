{ stdenv, git, cacert, haskellPackages, findutils, gnugrep, parallel }:

{ name ? "", pscJSON, sha256, meta ? {} }:

stdenv.mkDerivation {
  name = if name != "" then name else "fetchpsc";

  buildInputs = [
    git haskellPackages.purescript-native findutils gnugrep parallel
  ];

  phases = [ "buildPhase" "installPhase" ];

  buildPhase = ''
      cp ${pscJSON} ./psc-package.json
      psc-package update
      mv .psc-package pkgs
      find pkgs/ -print0 \
          | egrep -z "/[.](git|gitignore|set|travis.yml)$" \
          | parallel -0 "printf 'deleting %s\n' {}; rm -rf {};"
  '';

  installPhase = ''
      cp -R pkgs "$out"
  '';

  GIT_SSL_CAINFO = "${cacert}/etc/ssl/certs/ca-bundle.crt";

  outputHashMode = "recursive";
  outputHashAlgo = "sha256";
  outputHash = sha256;

  preferHashedMirrors = true;
  preferLocalBuild = true;

  inherit meta;
}