{ stdenv, fetchurl, binutilsCross ? null, gccCross ? null
, onlyHeaders ? false
, onlyPthreads ? false
}:

let src = fetchurl {
      url = "mirror://sourceforge/mingw-w64/mingw-w64-v5.0-rc2.tar.bz2";
      sha256 = "0imdary8j07if8ih73pfgxiclpf2ax8h3mz8mxln07i8sbbd30c9";
    };

    version = "5.0rc2";

    headersBuild = {
      name = "mingw-w64-headers-${version}";

      preConfigure = ''
        cd mingw-w64-headers
      '';
    };

    pthreadsBuild = {
      name = "mingw-w64-pthreads-${version}";

      preConfigure = ''
        cd mingw-w64-libraries/winpthreads
      '';
    };

    genericBuild = {
      name = "mingw-w64-${version}";

      buildInputs = [ gccCross binutilsCross ];

      crossConfig = gccCross.crossConfig;

      dontStrip = true;
    };

    common = {
      inherit src;

      configureFlags = [
        "--enable-idl"
        "--enable-secure-api"
      ];
    };

    specific = if      onlyHeaders  then headersBuild
               else if onlyPthreads then pthreadsBuild
               else                      genericBuild;

in stdenv.mkDerivation (common // specific)
