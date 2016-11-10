{ nixpkgs ? { outPath = <nixpkgs>; } }:

rec {
  root = rec {
    origPkgs = import nixpkgs {};
    oldOverrides = pkgs: {
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
    };

    commonPackageOverrides = self: super: {
      chat-doc = self.callPackage ./doc.nix {};
      chat-shaker = self.callPackage ./chat-shaker {};
      arcane-chat = self.qt56.callPackage ./chat {};

      libtoxcore-dev = self.callPackage ./fixes/libtoxcore/new-api {};

      protoc-gen-doc = self.qt56.callPackage ./fixes/protoc-gen-doc {};

      rtags = self.stdenv.mkDerivation {
        name = super.rtags.name;
        src = super.rtags.override { llvmPackages = self.llvmPackages_39; };
        buildInputs = [ self.makeWrapper ];
        buildPhase = "";
        installPhase = ''
            VERSION="${self.gcc.cc.version}"
            SYSTEM="$(basename $(dirname $(dirname $(${self.gcc.cc}/bin/g++ -print-libgcc-file-name))))"
            FLAGS=""
            FLAGS="$FLAGS -isystem ${self.gcc.libc}/include"
            FLAGS="$FLAGS -isystem ${self.gcc.cc}/include"
            FLAGS="$FLAGS -isystem ${self.gcc.cc}/include/c++/$VERSION"
            FLAGS="$FLAGS -isystem ${self.gcc.cc}/include/c++/$VERSION/$SYSTEM"
            FLAGS="$FLAGS -isystem ${self.gcc.cc}/include/c++/$VERSION/backward"
            FLAGS="$FLAGS -isystem ${self.gcc.cc}/lib/gcc/$SYSTEM/$VERSION/include"
            FLAGS="$FLAGS -isystem ${self.gcc.cc}/lib/gcc/$SYSTEM/$VERSION/include-fixed"
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

      include-what-you-use = super.include-what-you-use.overrideDerivation (old: {
        name = "include-what-you-use-3.8";

        src = self.fetchFromGitHub {
          repo   = "include-what-you-use";
          owner  = "include-what-you-use";
          rev    = "f09b88aaa0b7bb88a7b36da2ba1cab233659df6e";
          sha256 = "12ar5dgimyr4nqhn13kya0ijj6z73x8arf9gdnricx5i7fs83xxq";
        };

        buildInputs = old.buildInputs ++ [ self.python self.makeWrapper ];
        postInstall = ''
            patchShebangs $out/bin
            ln -s ${self.llvmPackages_38.clang-unwrapped}/lib $out/lib
            VERSION="${self.gcc.cc.version}"
            SYSTEM="$(basename $(dirname $(dirname $(${self.gcc.cc}/bin/g++ -print-libgcc-file-name))))"
            FLAGS=""
            FLAGS="$FLAGS -isystem ${self.gcc.libc}/include"
            FLAGS="$FLAGS -isystem ${self.gcc.cc}/include"
            FLAGS="$FLAGS -isystem ${self.gcc.cc}/include/c++/$VERSION"
            FLAGS="$FLAGS -isystem ${self.gcc.cc}/include/c++/$VERSION/$SYSTEM"
            FLAGS="$FLAGS -isystem ${self.gcc.cc}/include/c++/$VERSION/backward"
            FLAGS="$FLAGS -isystem ${self.gcc.cc}/lib/gcc/$SYSTEM/$VERSION/include"
            FLAGS="$FLAGS -isystem ${self.gcc.cc}/lib/gcc/$SYSTEM/$VERSION/include-fixed"
            FLAGS="$FLAGS \$NIX_CFLAGS_COMPILE"
            wrapProgram $out/bin/include-what-you-use --add-flags "$FLAGS"
        '';
      });

      gst_all_1 = self.recurseIntoAttrs (self.callPackage ./fixes/gstreamer {});

      libsigcxx_updated = super.libsigcxx.overrideDerivation (old: rec {
        name = "libsigc++-2.9.3";
        src = self.fetchurl {
          url = "mirror://gnome/sources/libsigc++/2.9/${name}.tar.xz";
          sha256 = "0zq963d0sss82q62fdfjs7l9iwbdch51albck18cb631ml0v7y8b";
        };
      });

      glib_updated = self.glib.overrideDerivation (old: rec {
        name = "glib-2.50.0";
        src = self.fetchurl {
          url = "mirror://gnome/sources/glib/2.50/${name}.tar.xz";
          sha256 = "0w3x3gq7hn4l93cn9kx84jwq43dvqnrhb4kj29pa1g96lqgma2w3";
        };

        configureFlags = old.configureFlags ++ [ "--disable-libmount" ];
      });

      glibmm_updated = self.glibmm.overrideDerivation (old: rec {
        name = "glibmm-2.50.0";
        src = self.fetchurl {
          url = "mirror://gnome/sources/glibmm/2.50/${name}.tar.xz";
          sha256 = "152yz5w0lx0y5j9ml72az7pc83p4l92bc0sb8whpcazldqy6wwnz";
        };
      });

      pythonPackagesGen = pp: self.callPackage ./fixes/pythonPackages.nix {
        inherit self super; pythonPackages = pp;
      };

      pypyPackages     = self.pythonPackagesGen super.pypyPackages;
      python27Packages = self.pythonPackagesGen super.python27Packages;
      python33Packages = self.pythonPackagesGen super.python33Packages;
      python34Packages = self.pythonPackagesGen super.python34Packages;
      python35Packages = self.pythonPackagesGen super.python35Packages;
      python2Packages  = self.python27Packages;
      python3Packages  = self.python34Packages;
      pythonPackages   = self.python27Packages;

      sphinxbase = self.callPackage ./fixes/sphinxbase.nix {};

      pocketsphinx = self.callPackage ./fixes/pocketsphinx {};
    };

    linuxPackageOverrides = self: super: rec {
      # # FIXME: add a deploy for linux here
      # deploy-linux = self.runCommand "arcane-chat-deploy-linux" {} ''
      # '';
    };

    windowsPackageOverrides = self: super: let
      overrideDerivation = self.stdenv.lib.overrideDerivation;
      overrideCrossDerivation = drv: fun: drv // {
        crossDrv = overrideDerivation drv.crossDrv fun;
      };
    in {
      deploy = self.runCommand "arcane-chat-deploy-windows" {
        buildInputs = [ self.zip ];
      } ''
          mkdir -pv $out/{nix-support,arcane-chat/platforms}
          cp -v ${self.arcane-chat.crossDrv}/bin/* \
                $out/arcane-chat/
          cp -v ${self.qt56.qtbase.crossDrv}/plugins/platforms/* \
                $out/arcane-chat/platforms
          cd $out
          cp -v $out/arcane-chat/arcane-chat.exe $out/
          # zip -v -1 -r $out/deploy.zip ./arcane-chat
          echo > $out/nix-support/hydra-build-products
          # echo "file deploy $out/deploy.zip" \
          #     >> $out/nix-support/hydra-build-products
          echo "file exec $out/arcane-chat.exe" \
              >> $out/nix-support/hydra-build-products
          rm -rvf $out/arcane-chat/
          du -h -d 0 $out
      '';

      chat-shaker = linux.super.chat-shaker;

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

      libtasn1 = super.libtasn1.override {
        perl = super.forceNativeDrv super.perl;
        texinfo = super.forceNativeDrv super.texinfo;
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
      help2man = super.forceNativeDrv super.help2man;
      intltool = super.forceNativeDrv super.intltool;
      perl = super.forceNativeDrv super.perl;
      utillinuxMinimal = super.forceNativeDrv super.utillinuxMinimal;
      mariadb = null;
      taglib = null;
      libavc1394 = null;
      libiec61883 = null;
      xorg = super.lib.attrsets.mapAttrs (k: v: null) super.xorg // {
        libXcursor = super.buildEnv { name = "libXcursor-dummy"; paths = []; };
        libX11 = super.buildEnv { name = "libX11-dummy"; paths = []; };
        lndir = super.xorg.lndir;
      };
      xlibs = super.lib.attrsets.mapAttrs (k: v: null) super.xlibs // {
        inherit (super.xorg) libXcursor libX11;
      };
      libXext = null;
      libxcb = null;
      libxkbcommon = null;
      python = super.forceNativeDrv super.python;
      cups = null;
      libusb1 = null;
      ghostscript = null;
      coreutils = super.forceNativeDrv super.coreutils;
      libpulseaudio = null;
      cmake = super.forceNativeDrv super.cmake;
      v4l_utils = null;
      libv4l = null;
      libvdpau = null;
      postgresql = null;
      vala = super.forceNativeDrv super.vala;
      yasm = super.forceNativeDrv super.yasm;
      ncurses = super.forceNativeDrv super.ncurses;
      guileCross = super.guile.crossDrv;
      guile = super.forceNativeDrv super.guile;
      bison = super.forceNativeDrv super.bison;
      bison2 = super.forceNativeDrv super.bison2;
      bison3 = super.forceNativeDrv super.bison3;
      flex = super.forceNativeDrv super.flex;
      flex_2_5_35 = super.forceNativeDrv super.flex_2_5_35;
      yacc = super.forceNativeDrv super.yacc;
      autogen = super.forceNativeDrv super.autogen;
      pkgconfig = super.forceNativeDrv super.pkgconfig;
      m4 = super.forceNativeDrv super.m4;
      gobjectIntrospection = super.forceNativeDrv super.gobjectIntrospection;
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

      gettext = overrideCrossDerivation super.gettext (old: {
        buildInputs = [ self.libiconv.crossDrv ];
      });

      x264 = overrideCrossDerivation super.x264 (old: {
        configureFlags = old.configureFlags ++ [
          "--cross-prefix=x86_64-w64-mingw32-"
        ];
      });

      giflib = super.giflib // {
        crossDrv = (super.giflib.override { xmlto = null; }).crossDrv;
      };

      qt56 = (
        let imported = import ./fixes/5.6 { pkgs = self; };
        in self.recurseIntoAttrs (imported.override (super: self.qt5LibsFun)));

      libmsgpack = self.callPackage ./fixes/libmsgpack.nix {};

      nlohmann_json = self.callPackage ./fixes/nlohmann_json.nix {};

      protobuf3_0 = super.callPackage ./fixes/protobuf3.nix {};

      faad2 = overrideCrossDerivation super.faad2 (old: {
        patches = [ ./fixes/faad2-frontend-off_t.patch ];
      });

      lame = overrideCrossDerivation super.lame (old: {
        patches = old.patches ++ [ ./fixes/lame-dbl-epsilon.patch ];
      });

      libtheora = overrideCrossDerivation super.libtheora (old: {
        configureFlags = [
          "--disable-examples"
          "--disable-shared"
          "--enable-static"
        ];
      });

      nettle = overrideCrossDerivation super.nettle (old: {
        nativeBuildInputs = old.nativeBuildInputs ++ [ self.m4 ];
      });

      gdk_pixbuf = overrideCrossDerivation super.gdk_pixbuf (old: {
        configureFlags = [
          "--disable-shared"
          "--enable-static"
          "--without-libjasper"
          "--without-x11"
        ];

        propagatedBuildInputs = [
          self.pkgconfig
          self.glib.crossDrv.dev
          self.libtiff.crossDrv.dev
          self.libjpeg.crossDrv.dev
          self.libpng.crossDrv.dev
        ];
      });

      libgsf = overrideCrossDerivation super.libgsf (old: {
        configureFlags = [
          "--disable-shared"
          "--enable-static"
          "--disable-introspection"
        ];

        patches = [ ./fixes/libgsf-dllmain.patch ];
      });

      dbus = overrideCrossDerivation super.dbus (old: {
        configureFlags = old.configureFlags ++ [
          "--disable-systemd"
          "--disable-shared"
          "--enable-static"
        ];
      });

      dbus_libs = self.dbus;
      dbus_tools = self.dbus;

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


      gst_all_1 = super.gst_all_1.override { fluidsynth = null; };

      glib = (
        let
          glibOverride = super.glib.override {
            libintlOrEmpty = [ self.gettext ];
          };
          glibOverrideCross = super.glib.override {
            libintlOrEmpty = [ self.gettext.crossDrv ];
          };
          glibFixed = overrideDerivation glibOverride (old: {
            configureFlags = old.configureFlags ++ [ "--with-libiconv=gnu" ];
          });
        in glibFixed // {
          crossDrv = overrideDerivation glibOverrideCross.crossDrv (old: {
            propagatedBuildInputs = old.propagatedBuildInputs ++ [
              self.windows.mingw_w64_pthreads.crossDrv
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

            NIX_CROSS_CFLAGS_COMPILE = [ "-gdwarf-2" "-gstrict-dwarf" ];
            dontCrossStrip = true;

            patches = old.patches ++ [
              ./fixes/glib/0001-Use-CreateFile-on-Win32-to-make-sure-g_unlink-always.patch
              ./fixes/glib/0004-glib-prefer-constructors-over-DllMain.patch
              ./fixes/glib/0027-no_sys_if_nametoindex.patch
              ./fixes/glib/0028-inode_directory.patch
              ./fixes/glib-debug.patch
            ];
          });
        });

      zeromq4 = super.zeromq4.override { libuuid = null; };

      glibmm = overrideCrossDerivation super.glibmm (old: {
        configureFlags = [
          "--enable-static"
          "--disable-shared"
          "--disable-documentation"
        ];

        nativeBuildInputs = old.nativeBuildInputs ++ [ self.glib.dev ];

        propagatedBuildInputs = old.propagatedBuildInputs ++ [
          self.windows.mingw-std-threads
          self.windows.mingw_w64_pthreads.crossDrv
        ];
      });
    };

    makeConfig = localOverrides: {
      packageOverrides = pkgs: (
        let
          common = commonPackageOverrides (pkgs // common // local) pkgs;
          local = localOverrides (pkgs // common);
        in common // local);
    };

    applyOverrides = pkgs: localOverrides: pkgs.lib.fix'
    (pkgs.lib.extends (utils.mergeOverrides commonPackageOverrides localOverrides) pkgs.__unfix__);
    #(pkgs.lib.extends localOverrides
    #  (pkgs.lib.extends commonPackageOverrides pkgs.__unfix__));
  };

  utils = {
    mergeOverrides = a: b: self: super:
    let
      aResult = (a self super);
      bResult = (b self (super // aResult));
    in aResult // bResult;
  };

  inherit (root) makeConfig;
  inherit (root) commonPackageOverrides;
  inherit (root) linuxPackageOverrides;
  inherit (root) windowsPackageOverrides;

  linuxPkgs = (root.origPkgs.overridePackages
    (utils.mergeOverrides commonPackageOverrides linuxPackageOverrides));
  linuxCallPackage = linuxPkgs.qt56.newScope linux;
  linux.super = linuxPkgs;

  windowsPkgs = (root.applyOverrides
    (import nixpkgs.outPath {
      crossSystem = {
        config         = "x86_64-w64-mingw32";
        arch           = "x86_64";
        libc           = "msvcrt";
        #platform       = {};
        openssl.system = "mingw64";
      };
      config.packageOverrides = root.oldOverrides;
    })
    windowsPackageOverrides);
  windowsCallPackage = windowsPkgs.qt56.newScope windows;
  windows.super = windowsPkgs;

  darwinPkgs = import nixpkgs.outPath { system = "x86_64-darwin"; };
  darwinCallPackage = darwinPkgs.newScope darwin;
  darwin.super = darwinPkgs;
}
