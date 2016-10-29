{ self, super, pythonPackages }:

with (builtins // {
  pkgs = self;
  lib = self.lib;
  pp = pythonPackages;
});

let removeNulls = lib.filterAttrs (n: v: v != null);
    srcHelper = url: sha256: curlOpts: name:
      assert url != null;
      assert sha256 != null;
      pkgs.fetchurl (removeNulls { inherit url sha256 name curlOpts; });

    mkPython =
      { packageName            # < A package name.
      , version                # < A package version.
      , srcURL          ? null # < A url to download from.
      , srcSHA          ? null # < A SHA256 checksum for the download.
      , srcName         ? null # < A name for the downloaded file.
      , srcCurlOpts     ? null # < A curl options for the download.
      , src             ? null # < A source to use (overrides other options).
      , buildDeps       ? []   # < Build dependencies to add.
      , propDeps        ? []   # < Propagated dependencies to add.
      , nativeBuildDeps ? []   # < Native build dependencies to add.
      , nativePropDeps  ? []   # < Native propagated dependencies to add.
      # The following attributes are equivalent to those in meta:
      , homepage        ? null # < A homepage of the project.
      , description     ? null # < A description.
      , longDescription ? null # < A longer description.
      , maintainers     ? null # < A list of maintainers.
      , license         ? null # < A license.
      , platforms       ? null # < A list of supported platforms.
      , ... # Any other options will be passed to buildPythonPackage directly
      } @ args:

      pp.buildPythonPackage (removeNulls ({
        name = "${packageName}-${version}";

        src = if isNull src
              then srcHelper srcURL srcSHA srcCurlOpts srcName
              else src;

        buildInputs           = buildDeps;
        propagatedBuildInputs = propDeps;

        nativeBuildInputs           = nativeBuildDeps;
        propagatedNativeBuildInputs = nativePropDeps;

        meta = removeNulls {
          inherit homepage description longDescription
                  license maintainers platforms;
        };
      } // args));
   isNonEmptyString = x: stringLength x > 0;
   strHead = x: (assert isNonEmptyString x; substring 0 1 x);
   pypiURL = name: version:
     assert isNonEmptyString name;
     assert isNonEmptyString version;
     "mirror://pypi/${strHead name}/${name}/${name}-${version}.tar.gz";

in pp // rec {
  gst-gtklaunch = mkPython (rec {
    packageName = "gst-gtklaunch";
    version = "20160115";

    src = pkgs.fetchFromGitHub {
      owner  = "UbiCastTeam";
      repo   = "gst-gtklaunch-1.0";
      rev    = "f53b1eb2a5ee089eef641d885103308411712f0e";
      sha256 = "1ik4n20splppq0zjgi3ixmab7vq9yy7m6cih08jw5bf334lnznvd";
    };

    propDeps = with pkgs; [
      gtk3
      pygobject3
      graphviz
      gobjectIntrospection
      gst_all_1.gstreamer
      gst_all_1.gst-plugins-base
      gst_all_1.gst-plugins-good
      gst_all_1.gst-plugins-ugly
      gst_all_1.gst-plugins-bad
    ];

    makeWrapperArgs = [
      "--prefix GI_TYPELIB_PATH : \"$GI_TYPELIB_PATH\""
      "--prefix GST_PLUGIN_SYSTEM_PATH_1_0 : \"$GST_PLUGIN_SYSTEM_PATH_1_0\""
      "--prefix PATH : \"${pkgs.graphviz}/bin:${pkgs.pythonPackages.xdot}/bin\""
    ];

    homepage     = "https://github.com/UbiCastTeam/gst-gtklaunch-1.0";
    description  = "A utility for testing GStreamer pipelines and elements";
    license      = with lib.licenses; [ lgpl21.spdxId ];
    maintainers  = with lib.maintainers; [ taktoa ];
    platforms    = with lib.platforms; all;
  });

  gitlint = mkPython (rec {
    packageName = "gitlint";
    version     = "0.7.1";
    srcURL      = pypiURL packageName version;
    srcSHA      = "0b5whch634bz683s2p76l6wqxahpqkk355095b2m7v845wmky3dv";
    propDeps    = with pp; [ ordereddict sh click_5 ];
    homepage    = "https://github.com/jorisroovers/gitlint";
    description = "A Git commit message linter";
    license     = with lib.licenses; [ mit.spdxId ];
    maintainers = with lib.maintainers; [ taktoa ];
    platforms   = with lib.platforms; all;
  });

  typed-ast = mkPython (rec {
    packageName  = "typed-ast";
    version      = "0.5.3";
    srcURL       = pypiURL packageName version;
    srcSHA       = "0ybssbjbmx311w9jyix1ig9kkhn85n1yy0br1sa8ipp9ajgs8dcn";
    propDeps     = [];
    homepage     = "https://github.com/dropbox/typed_ast";
    description  = "A typed AST for Python";
    license      = with lib.licenses; [ asl20.spdxId ];
    maintainers  = with lib.maintainers; [ taktoa ];
    platforms    = with lib.platforms; all;
  });

  mypy-lang = mkPython (rec {
    packageName  = "mypy-lang";
    version      = "0.4.1";
    srcURL       = pypiURL packageName version;
    srcSHA       = "0xqvp88fmjbykjdcp0a2smlnmhsqz8m6x96drc34v9bkqms29vhw";
    propDeps     = [ typed-ast ];
    homepage     = "http://mypy-lang.org";
    description  = "Experimental static types for Python";
    license      = with lib.licenses; [ mit.spdxId ];
    maintainers  = with lib.maintainers; [ taktoa ];
    platforms    = with lib.platforms; all;
  });

  ptpython = mkPython (rec {
    packageName  = "ptpython";
    version      = "0.34";
    srcURL       = pypiURL packageName version;
    srcSHA       = "1mmbiyzf0n8hm7z2a562x7w5cbl6jc0zsk6vp40q1z4cyblv1k13";
    propDeps     = with pp; [ jedi docopt pygments prompt_toolkit ];
    homepage     = "https://github.com/jonathanslenders/ptpython";
    description  = "A more advanced Python REPL";
    license      = with lib.licenses; [ bsd3.spdxId ];
    maintainers  = with lib.maintainers; [ taktoa ];
    platforms    = with lib.platforms; all;
    doCheck      = false;
  });

  pocketsphinx = mkPython (rec {
    packageName  = "pocketsphinx";
    version      = "0.1.3";
    srcURL       = pypiURL packageName version;
    srcSHA       = "0v9l43s6s256qy4bs31akvmqkkrxdkk8mld40s3vfnn8xynml4mc";
    propDeps     = [ pkgs.swig pkgs.libpulseaudio pkgs.pocketsphinx ];
    doCheck      = false;
  });

  SpeechRecognition = mkPython (rec {
    packageName  = "SpeechRecognition";
    version      = "3.4.6";
    srcURL       = pypiURL packageName version;
    srcSHA       = "0cdci4nxyyfg2jrzhrnm3hvqcb1w8w3gym6dr9nl1rh2y4ssngir";
    propDeps     = with pp; [ pyaudio pkgs.flac pocketsphinx ];
    homepage     = "https://github.com/Uberi/speech_recognition";
    description  = "Speech recognition library";
    license      = with lib.licenses; [ bsd3.spdxId ];
    maintainers  = with lib.maintainers; [ taktoa ];
    platforms    = with lib.platforms; all;
    doCheck      = false;
  });
}
