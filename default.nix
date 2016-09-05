{ nixpkgs ? { outPath = <nixpkgs>; } }:

rec {
  linuxPkgs = import nixpkgs.outPath {};
  linuxCallPackage = linuxPkgs.qt57.newScope linux;
  linux = rec {
    # Boilerplate
    super = linuxPkgs;

    # Our packages
    gst_all_1 = super.recurseIntoAttrs (linuxCallPackage ./fixes/gstreamer {});
    fuspr-chat = linuxCallPackage ./chat {};
  };

  windowsPkgs = import nixpkgs.outPath {
    crossSystem = {
      config         = "x86_64-w64-mingw32";
      arch           = "x86_64";
      libc           = "msvcrt";
      platform       = {};
      openssl.system = "mingw64";
    };
  };
  windowsCallPackage = windowsPkgs.qt57.newScope windows;
  windows = rec {
    # Boilerplate
    super = windowsPkgs;

    # Overrides
    gettext = super.gettext.overrideDerivation (old: {
      buildInputs = [ super.libiconv.crossDrv ];
    });
    libmsgpack = windowsCallPackage ./fixes/libmsgpack.nix {};
    nlohmann_json = windowsCallPackage ./fixes/nlohmann_json.nix {};
    libvpx = super.libvpx.override {
      stdenv = super.stdenv // {
        isCygwin = true;
      };
      unitTestsSupport = true;
      webmIOSupport = true;
      libyuvSupport = true;
    };

    # Our packages
    fuspr-chat = windowsCallPackage ./chat {};
  };

  darwinPkgs = import nixpkgs.outPath { system = "x86_64-darwin"; };
  darwinCallPackage = darwinPkgs.newScope darwin;
  darwin = rec {
    # Boilerplate
    super = darwinPkgs;

    # Our packages
    fuspr-chat = darwinCallPackage ./chat { qtbase = darwinPkgs.qt48Full; };
  };
}
