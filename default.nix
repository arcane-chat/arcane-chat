{ nixpkgs ? import <nixpkgs> }:

rec {
  linuxPkgs = nixpkgs {};
  linuxCallPackage = linuxPkgs.newScope linux;
  linux = rec {
    # Boilerplate
    super = linuxPkgs;

    # Our packages
    fuspr-chat = linuxCallPackage ./chat {};
  };

  windowsPkgs = nixpkgs {
    crossSystem = {
      config         = "x86_64-w64-mingw32";
      arch           = "x86_64";
      libc           = "msvcrt";
      platform       = {};
      openssl.system = "mingw64";
    };
  };
  windowsCallPackage = windowsPkgs.newScope windows;
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

  darwinPkgs = nixpkgs {}; # FIXME
  darwinCallPackage = darwinPkgs.newScope darwin;
  darwin = rec {
    # Boilerplate
    super = darwinPkgs;

    # Our packages
    fuspr-chat = darwinCallPackage ./chat {};
  };
}
