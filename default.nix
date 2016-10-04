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

    windows = pkgs.windows // {
      mingw_w64 = pkgs.callPackage ./fixes/mingw-w64.nix {
        gccCross = pkgs.gccCrossStageStatic;
        binutilsCross = pkgs.binutilsCross;
      };

      mingw_w64_headers = pkgs.callPackage ./fixes/mingw-w64.nix {
        onlyHeaders = true;
      };

      mingw_w64_pthreads = pkgs.callPackage ./fixes/mingw-w64.nix {
        onlyPthreads = true;
      };

      mingw-std-threads = pkgs.stdenv.mkDerivation rec {
        name = "mingw-std-threads-20160912";

        src = pkgs.fetchFromGitHub {
          owner = "meganz";
          repo = "mingw-std-threads";
          rev = "d81ca0b7514a0ded6c329f84be9e5f07829d2418";
          sha256 = "13qf3rb25d3c7z8jrclh9mxv6qs28kzymvag7nr9j25jq0j81r0d";
        };

        inherit (pkgs.stdenv.cc) cc;

        buildPhase = ''
            local INCLUDE
            INCLUDE="${cc.out}/include/c++/${cc.version}"

            function footer () {
                printf "\n"
                printf "#ifdef THREAD_PRIORITY_NORMAL\n"
                printf "#undef THREAD_PRIORITY_NORMAL\n"
                printf "#endif\n"
            }

            printf "#include \"%s\"\n" "$INCLUDE/mutex" > mutex

            { cat "mingw.condition_variable.h"; footer; } >> condition_variable
            { cat "mingw.thread.h";             footer; } >> thread
            { cat "mingw.mutex.h";              footer; } >> mutex
        '';

        installPhase = ''
            mkdir -p $out/include
            cp condition_variable $out/include/
            cp mutex              $out/include/
            cp thread             $out/include/
        '';
      };
    };

    gst_all_1 = pkgs.recurseIntoAttrs (pkgs.callPackage ./fixes/gstreamer {});

    pango = pkgs.callPackage ./fixes/pango.nix {};

    libsigcxx = pkgs.libsigcxx.overrideDerivation (old: rec {
      name = "libsigc++-2.9.3";
      src = pkgs.fetchurl {
        url = "mirror://gnome/sources/libsigc++/2.9/${name}.tar.xz";
        sha256 = "0zq963d0sss82q62fdfjs7l9iwbdch51albck18cb631ml0v7y8b";
      };
    });

    glib = pkgs.glib.overrideDerivation (old: rec {
      name = "glib-2.50.0";
      src = pkgs.fetchurl {
        url = "mirror://gnome/sources/glib/2.50/${name}.tar.xz";
        sha256 = "0w3x3gq7hn4l93cn9kx84jwq43dvqnrhb4kj29pa1g96lqgma2w3";
      };

      configureFlags = old.configureFlags ++ [ "--disable-libmount" ];
    });

    glibmm = pkgs.glibmm.overrideDerivation (old: rec {
      name = "glibmm-2.50.0";
      src = pkgs.fetchurl {
        url = "mirror://gnome/sources/glibmm/2.50/${name}.tar.xz";
        sha256 = "152yz5w0lx0y5j9ml72az7pc83p4l92bc0sb8whpcazldqy6wwnz";
      };
    });
  };

  linuxPackageOverrides = pkgs: rec {
  };

  windowsPackageOverrides = pkgs: let
    self = rec {
      overrideDerivation = pkgs.stdenv.lib.overrideDerivation;

      overrideCrossDerivation =
        drv: fun: drv // { crossDrv = overrideDerivation drv.crossDrv fun; };

      # cairo = overrideCrossDerivation (pkgs.cairo.override {
      #   xcbSupport = false;
      #   glSupport = false;
      #   xorg = {
      #     libXext = null;
      #     libXrender = null;
      #     pixman = null;
      #   };
      # }) (old: {
      #   patches =
      # });

      libtasn1 = pkgs.libtasn1.override {
        perl = pkgs.forceNativeDrv pkgs.perl;
        texinfo = pkgs.forceNativeDrv pkgs.texinfo;
      };

      # the ugly fixes
      mesaSupported = false;
      x11Support = false;
      cupsSupport = false;
      xcbSupport = false;
      glSupport = false;

      ruby = null;
      libcdio = null;
      lzip = null;
      systemd = null;
      help2man = pkgs.forceNativeDrv pkgs.help2man;
      intltool = pkgs.forceNativeDrv pkgs.intltool;
      perl = pkgs.forceNativeDrv pkgs.perl;
      mariadb = null;
      taglib = null;
      libavc1394 = null;
      libiec61883 = null;
      xorg = pkgs.lib.attrsets.mapAttrs (k: v: null) pkgs.xorg // {
        libXcursor = pkgs.buildEnv { name = "libXcursor-dummy"; paths = []; };
        libX11 = pkgs.buildEnv { name = "libX11-dummy"; paths = []; };
      };
      xlibs = pkgs.lib.attrsets.mapAttrs (k: v: null) pkgs.xlibs // {
        inherit (xorg) libXcursor libX11;
      };
      libXext = null;
      libxcb = null;
      libxkbcommon = null;
      python = pkgs.forceNativeDrv pkgs.python;
      cups = null;
      libusb1 = null;
      ghostscript = null;
      coreutils = pkgs.forceNativeDrv pkgs.coreutils;
      libpulseaudio = null;
      cmake = pkgs.forceNativeDrv pkgs.cmake;
      v4l_utils = null;
      libv4l = null;
      libvdpau = null;
      postgresql = null;
      vala = pkgs.forceNativeDrv pkgs.vala;
      yasm = pkgs.forceNativeDrv pkgs.yasm;
      ncurses = pkgs.forceNativeDrv pkgs.ncurses;
      guile = pkgs.forceNativeDrv pkgs.guile;
      bison = pkgs.forceNativeDrv pkgs.bison;
      bison2 = pkgs.forceNativeDrv pkgs.bison2;
      bison3 = pkgs.forceNativeDrv pkgs.bison3;
      flex = pkgs.forceNativeDrv pkgs.flex;
      flex_2_5_35 = pkgs.forceNativeDrv pkgs.flex_2_5_35;
      yacc = pkgs.forceNativeDrv pkgs.yacc;
      autogen = pkgs.forceNativeDrv pkgs.autogen;
      pkgconfig = pkgs.forceNativeDrv pkgs.pkgconfig;
      m4 = pkgs.forceNativeDrv pkgs.m4;
      gobjectIntrospection = pkgs.forceNativeDrv pkgs.gobjectIntrospection;
      libgnome_keyring = null;
      libgnome_keyring3 = null;
      speex = null;
      pango = null;
      cairo = null;
      mesa = null;
      freetype = null;
      fontconfig = null;
      icu = null;
      harfbuzz = null;
      harfbuzz-icu = null;
      libass = null;
      libvpx = null;
      a52dec = null;
      libcaca = null;
      aalib = null;
      libshout = null;
      openjpeg = null;
      libdvdread = null;
      librsvg = null;
      libmpeg2 = null;
      mjpegtools = null;
      mjpegtoolsFull = null;
      libwebp = null;
      wildmidi = null;
      libedit = null;
      libdv = null;
      libsoup = null;
      gnutls = null;
      alsaLib = null;
      wayland = null;
      include-what-you-use = null;
      rtags = null;

      gettext = overrideCrossDerivation pkgs.gettext (old: {
        buildInputs = [ pkgs.libiconv.crossDrv ];
      });

      x264 = overrideCrossDerivation pkgs.x264 (old: {
        configureFlags = old.configureFlags ++ [
          "--cross-prefix=x86_64-w64-mingw32-"
        ];
      });

      giflib = pkgs.giflib // {
        crossDrv = (pkgs.giflib.override { xmlto = null; }).crossDrv;
      };

      #qt56 = pkgs.qt56 // {
      #  qtbase = overrideCrossDerivation pkgs.qt56.qtbase (old: {
      #    configureFlags = old.configureFlags +
      #      ''
      #        -xplatform win32-g++
      #        -device-option CROSS_COMPILE=x86_64-w64-mingw32-
      #        -v
      #        -continue
      #        -qt-xcb
      #      '';
      #    dontSetConfigureCross=true;
      #  });
      #};

      # uses the wrong pkgs
      qt56 =
        let imported = import ./fixes/5.6 { pkgs = pkgs // self; };
        in pkgs.recurseIntoAttrs (imported.override (super: pkgs.qt5LibsFun));

      libmsgpack = pkgs.callPackage ./fixes/libmsgpack.nix {};

      nlohmann_json = pkgs.callPackage ./fixes/nlohmann_json.nix {};

      protobuf3_0 = pkgs.protobuf3_0.overrideDerivation (old: {
        doCheck = false;
        nativeBuildInputs = [ pkgs.autoreconfHook ];
        buildInputs = [ pkgs.zlib.crossDrv pkgs.libtool.crossDrv.lib ];
      });

      # libvpx = pkgs.libvpx.override {
      #   stdenv = pkgs.stdenv // { isCygwin = true; };
      #   unitTestsSupport = true;
      #   webmIOSupport = true;
      #   libyuvSupport = true;
      # };

      faad2 = overrideCrossDerivation pkgs.faad2 (old: {
        patches = [ ./fixes/faad2-frontend-off_t.patch ];
      });

      lame = overrideCrossDerivation pkgs.lame (old: {
        patches = old.patches ++ [ ./fixes/lame-dbl-epsilon.patch ];
      });

      libtheora = overrideCrossDerivation pkgs.libtheora (old: {
        configureFlags = [
          "--disable-examples"
          "--disable-shared"
          "--enable-static"
        ];
      });

      nettle = overrideCrossDerivation pkgs.nettle (old: {
        nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.m4 ];
      });

      gdk_pixbuf = overrideCrossDerivation pkgs.gdk_pixbuf (old: {
        configureFlags = [
          "--disable-shared"
          "--enable-static"
          "--without-libjasper"
          "--without-x11"
        ];

        propagatedBuildInputs = [
          pkgs.pkgconfig
          glib.crossDrv.dev
          pkgs.libtiff.crossDrv.dev
          pkgs.libjpeg.crossDrv.dev
          pkgs.libpng.crossDrv.dev
        ];
      });

      libgsf = overrideCrossDerivation pkgs.libgsf (old: {
        configureFlags = [
          "--disable-shared"
          "--enable-static"
          "--disable-introspection"
        ];

        patches = [ ./fixes/libgsf-dllmain.patch ];
      });

      dbus = overrideCrossDerivation pkgs.dbus (old: {
        configureFlags = old.configureFlags ++ [
          "--disable-systemd"
          "--disable-shared"
          "--enable-static"
        ];
      });

      dbus_libs = dbus;
      dbus_tools = dbus;

      # freetype = overrideCrossDerivation pkgs.freetype (old: {
      #   configureFlags = [
      #     "--disable-shared"
      #     "--enable-static"
      #   ];
      #
      #   postInstall = ''
      #       mkdir -p $dev/bin/
      #       mv -v $out/bin/freetype-config $dev/bin/
      #       rmdir --ignore-fail-on-non-empty $out/bin
      #   '';
      # });

      # fontconfig = overrideCrossDerivation pkgs.fontconfig (old: {
      #   configureFlags = [
      #     "--disable-shared"
      #     "--enable-static"
      #   ];
      # });


      gst_all_1 = pkgs.gst_all_1.override { fluidsynth = null; };

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
            propagatedBuildInputs = old.propagatedBuildInputs ++ [
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
            #     echo "\e[31mReconfiguring with shared library support\e[0m"
            #     export configureFlags="$configureFlags --enable-shared"
            #     configurePhase
            #     printf "\n\n\n\n\n\n"
            #     echo "\e[31mRebuilding with shared library support\e[0m"
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

      zeromq4 = let zmq4 = (pkgs.zeromq4.override { libuuid = null; });
                in overrideCrossDerivation zmq4 (old: {
                     patches = [
                       ./fixes/zeromq/includes-consistent.patch
                       ./fixes/zeromq/winxp-compatibility.patch
                     ];
                   });

      glibmm = overrideCrossDerivation pkgs.glibmm (old: {
        configureFlags = [
          "--enable-static"
          "--disable-shared"
          "--disable-documentation"
        ];

        nativeBuildInputs = old.nativeBuildInputs ++ [ glib.dev ];

        propagatedBuildInputs = old.propagatedBuildInputs ++ [
          pkgs.windows.mingw-std-threads
          pkgs.windows.mingw_w64_pthreads.crossDrv
        ];
      });
    };
  in self;

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
