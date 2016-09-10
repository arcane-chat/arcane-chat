{ nixpkgs ? { outPath = <nixpkgs>; } }:

let
  config = {
    packageOverrides = pkgs2: {
      libtoxcore-dev = pkgs2.libtoxcore-dev.overrideDerivation (oldAttrs: {
        src = pkgs2.fetchFromGitHub {
          owner = "TokTok";
          repo = "toxcore";
          rev = "05f474b4df8171412237f46c943822edd202b4a9";
          sha256 = "1wq0nbdcq125gcg7pqwqwa0pvh7zg78drd2f585b0a00m1rhzpdy";
        };
        patches = [ ./toxcore.patch ];
      });
    };
  };
in rec {
  linuxPkgs = import nixpkgs.outPath { inherit config; };
  linuxCallPackage = linuxPkgs.qt56.newScope linux;
  linux = rec {
    # Boilerplate
    super = linuxPkgs;

    # Our packages
    gst_all_1 = super.recurseIntoAttrs (linuxCallPackage ./fixes/gstreamer {});
    arcane-chat = linuxCallPackage ./chat {};
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
  windowsCallPackage = windowsPkgs.qt56.newScope windows;
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
    arcane-chat = windowsCallPackage ./chat {};
  };

  darwinPkgs = import nixpkgs.outPath { system = "x86_64-darwin"; };
  darwinCallPackage = darwinPkgs.newScope darwin;
  darwin = rec {
    # Boilerplate
    super = darwinPkgs;

    # Our packages
    arcane-chat = darwinCallPackage ./chat { qtbase = darwinPkgs.qt48Full; };
  };
}
