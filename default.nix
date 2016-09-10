{ nixpkgs ? { outPath = <nixpkgs>; } }:

let
  config = {
    packageOverrides = pkgs: {
      libtoxcore-dev = pkgs.libtoxcore-dev.overrideDerivation (old: {
        src = pkgs.fetchFromGitHub {
          owner = "TokTok";
          repo = "toxcore";
          rev = "05f474b4df8171412237f46c943822edd202b4a9";
          sha256 = "1wq0nbdcq125gcg7pqwqwa0pvh7zg78drd2f585b0a00m1rhzpdy";
        };
        patches = [ ./toxcore.patch ];
      });

      rtags = pkgs.stdenv.mkDerivation {
        name = pkgs.rtags.name;
        src = pkgs.rtags;
        buildInputs = [ pkgs.makeWrapper ];
        buildPhase = "";
        installPhase = ''
            VERSION="${pkgs.gcc.cc.version}"
            SYSTEM="$(basename $(dirname $(dirname $(${pkgs.gcc.cc}/bin/g++ -print-libgcc-file-name))))"
            FLAGS=""
            FLAGS="$FLAGS -isystem ${pkgs.gcc.libc}/include"
            FLAGS="$FLAGS -isystem ${pkgs.gcc.cc}/include"
            FLAGS="$FLAGS -isystem ${pkgs.gcc.cc}/include/c++/$VERSION"
            FLAGS="$FLAGS -isystem ${pkgs.gcc.cc}/include/c++/$VERSION/$SYSTEM"
            FLAGS="$FLAGS -isystem ${pkgs.gcc.cc}/include/c++/$VERSION/backward"
            FLAGS="$FLAGS -isystem ${pkgs.gcc.cc}/lib/gcc/$SYSTEM/$VERSION/include"
            FLAGS="$FLAGS -isystem ${pkgs.gcc.cc}/lib/gcc/$SYSTEM/$VERSION/include-fixed"
            FLAGS="$FLAGS \$NIX_CFLAGS_COMPILE"
            mkdir -p $out/bin
            cp ./bin/rdm $out/bin/rdm
            cp ./bin/rp $out/bin/rp
            cp ./bin/rc $out/bin/rc
            cp -R ./share $out/
            wrapProgram $out/bin/rdm \
                --add-flags "\$(echo $FLAGS | sed 's/-isystem/--isystem/g')"
        '';
      };

      include-what-you-use = pkgs.include-what-you-use.overrideDerivation (old: {
        name = "include-what-you-use-3.8";

        src = pkgs.fetchFromGitHub {
          repo   = "include-what-you-use";
          owner  = "include-what-you-use";
          rev    = "f09b88aaa0b7bb88a7b36da2ba1cab233659df6e";
          sha256 = "12ar5dgimyr4nqhn13kya0ijj6z73x8arf9gdnricx5i7fs83xxq";
        };

        buildInputs = old.buildInputs ++ [ pkgs.python pkgs.makeWrapper ];
        postInstall = ''
            patchShebangs $out/bin
            ln -s ${pkgs.llvmPackages_38.clang-unwrapped}/lib $out/lib
            VERSION="${pkgs.gcc.cc.version}"
            SYSTEM="$(basename $(dirname $(dirname $(${pkgs.gcc.cc}/bin/g++ -print-libgcc-file-name))))"
            FLAGS=""
            FLAGS="$FLAGS -isystem ${pkgs.gcc.libc}/include"
            FLAGS="$FLAGS -isystem ${pkgs.gcc.cc}/include"
            FLAGS="$FLAGS -isystem ${pkgs.gcc.cc}/include/c++/$VERSION"
            FLAGS="$FLAGS -isystem ${pkgs.gcc.cc}/include/c++/$VERSION/$SYSTEM"
            FLAGS="$FLAGS -isystem ${pkgs.gcc.cc}/include/c++/$VERSION/backward"
            FLAGS="$FLAGS -isystem ${pkgs.gcc.cc}/lib/gcc/$SYSTEM/$VERSION/include"
            FLAGS="$FLAGS -isystem ${pkgs.gcc.cc}/lib/gcc/$SYSTEM/$VERSION/include-fixed"
            FLAGS="$FLAGS \$NIX_CFLAGS_COMPILE"
            wrapProgram $out/bin/include-what-you-use --add-flags "$FLAGS"
        '';
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
