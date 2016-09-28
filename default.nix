{ nixpkgs ? { outPath = <nixpkgs>; } }:

let
  commonPackageOverrides = pkgs: rec {
    libtoxcore-dev = pkgs.libtoxcore-dev.overrideDerivation (old: {
      src = pkgs.fetchFromGitHub {
        owner = "TokTok";
        repo = "toxcore";
        rev = "05f474b4df8171412237f46c943822edd202b4a9";
        sha256 = "1wq0nbdcq125gcg7pqwqwa0pvh7zg78drd2f585b0a00m1rhzpdy";
      };
      patches = [ ./toxcore.patch ];
      buildInputs = with pkgs; [ libsodium ];
      nativeBuildInputs = with pkgs; [ autoreconfHook pkgconfig ];
      propagatedBuildInputs = [];
      propagatedNativeBuildInputs = [];
    });

    rtags = pkgs.stdenv.mkDerivation {
      name = pkgs.rtags.name;
      src = pkgs.rtags.override { llvmPackages = pkgs.llvmPackages_39; };
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

    gst_all_1 = pkgs.recurseIntoAttrs (pkgs.callPackage ./fixes/gstreamer {});
  };

  linuxPackageOverrides = pkgs: rec {
  };

  windowsPackageOverrides = pkgs: rec {
    cairo = pkgs.cairo.override {
      xcbSupport = false;
      glSupport = false;
      xorg = {
        libXext = null;
        libXrender = null;
        pixman = null;
      };
    };

    gettext = pkgs.gettext // {
      crossDrv = pkgs.stdenv.lib.overrideDerivation pkgs.gettext.crossDrv (old: {
        buildInputs = [ pkgs.libiconv.crossDrv ];
      });
    };

    libmsgpack = pkgs.callPackage ./fixes/libmsgpack.nix {};

    nlohmann_json = pkgs.callPackage ./fixes/nlohmann_json.nix {};

    #protobuf3_0 = pkgs.callPackage ./fixes/protobuf.nix {};
    protobuf3_0 = pkgs.protobuf3_0.overrideDerivation (old: with pkgs; {
      doCheck = false;
      nativeBuildInputs = [ autoreconfHook ];
      buildInputs = [ zlib.crossDrv libtool.crossDrv.lib ];
    });

    libvpx = pkgs.libvpx.override {
      stdenv = pkgs.stdenv // { isCygwin = true; };
      unitTestsSupport = true;
      webmIOSupport = true;
      libyuvSupport = true;
    };

    gst_all_1 = pkgs.gst_all_1.override {
      fluidsynth = null;
    };

    glib =
      let inherit (pkgs.stdenv.lib) overrideDerivation;
          glibOverride = pkgs.glib.override {
            libintlOrEmpty = [ gettext ];
          };
          glibOverrideCross = pkgs.glib.override {
            libintlOrEmpty = [ gettext.crossDrv ];
          };
          glibFixed = overrideDerivation glibOverride (old: {
            configureFlags = old.configureFlags ++ [ "--with-libiconv=gnu" ];
          });
      in glibFixed // {
        crossDrv = overrideDerivation glibOverrideCross.crossDrv (old: {
          CPPFLAGS = " -DMINGW_HAS_SECURE_API=1 ";

          buildInputs = old.buildInputs ++ [
            pkgs.windows.mingw_w64_pthreads.crossDrv
          ];

          dontDisableStatic = true;

          configureFlags = old.configureFlags ++ [
            "--enable-static"
            "--disable-shared"
            "--disable-libelf"
            "--with-threads=posix"
            "--with-libiconv=gnu"
            "--disable-installed-tests"
          ];

          # postBuild = ''
          #     printf "\n\n\n\n\n\n"
          #     echo "*** Rebuilding with shared library support ***"
          #     export configureFlags="$configureFlags --disable-static --enable-shared"
          #     configurePhase
          #     buildPhase
          # '';

          patches = old.patches ++ [
            ./fixes/glib/0001-Use-CreateFile-on-Win32-to-make-sure-g_unlink-always.patch
            ./fixes/glib/0004-glib-prefer-constructors-over-DllMain.patch
            ./fixes/glib/0027-no_sys_if_nametoindex.patch
            ./fixes/glib/0028-inode_directory.patch
          ];
        });
      };

    glibmm = pkgs.glibmm // {
      crossDrv = pkgs.stdenv.lib.overrideDerivation pkgs.glibmm.crossDrv (old: {
        configureFlags = [
          "--enable-static"
          "--disable-shared"
          "--disable-documentation"
        ];
        nativeBuildInputs = old.nativeBuildInputs ++ [ glib.dev ];
        buildInputs = old.buildInputs ++ [
          mingw-std-threads.crossDrv
          pkgs.windows.mingw_w64_pthreads.crossDrv
        ];
      });
    };

    mingw-std-threads = pkgs.stdenv.mkDerivation rec {
      name = "mingw-std-threads-20160912";

      src = pkgs.fetchFromGitHub {
        owner = "meganz";
        repo = "mingw-std-threads";
        rev = "d81ca0b7514a0ded6c329f84be9e5f07829d2418";
        sha256 = "13qf3rb25d3c7z8jrclh9mxv6qs28kzymvag7nr9j25jq0j81r0d";
      };

      dontBuild = true;

      installPhase = ''
          mkdir -p $out/include
          cp mingw.condition_variable.h $out/include/condition_variable
          cp mingw.mutex.h              $out/include/mutex
          cp mingw.thread.h             $out/include/thread
      '';
    };

  };

  makeConfig = localOverrides: {
    packageOverrides = pkgs: let common = commonPackageOverrides pkgs;
                                 local = localOverrides (pkgs // common);
                             in common // local;
  };

in rec {
  inherit makeConfig
          commonPackageOverrides
          linuxPackageOverrides
          windowsPackageOverrides;

  linuxPkgs = import nixpkgs.outPath {
    config = makeConfig linuxPackageOverrides;
  };
  linuxCallPackage = linuxPkgs.qt56.newScope linux;
  linux = rec {
    # Boilerplate
    super = linuxPkgs;

    # Our packages
    arcane-chat = linuxCallPackage ./chat {};
  };

  windowsPkgs = import nixpkgs.outPath {
    crossSystem = {
      config         = "x86_64-w64-mingw32";
      arch           = "x86_64";
      libc           = "msvcrt";
      #platform       = {};
      openssl.system = "mingw64";
    };
    config = makeConfig windowsPackageOverrides;
  };
  windowsCallPackage = windowsPkgs.qt56.newScope windows;
  windows = rec {
    # Boilerplate
    super = windowsPkgs;

    # Our packages
    arcane-chat = windowsCallPackage ./chat {
      doxygen = null;
      obs-studio = null;
      include-what-you-use = null;
    };
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
