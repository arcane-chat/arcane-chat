{ nixpkgs ? { outPath = <nixpkgs>; } }:
#{ nixpkgs ? ({ outPath = ./nixpkgs; }) }:

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

    gst_all_1 = pkgs.recurseIntoAttrs (pkgs.callPackage ./fixes/gstreamer {});

    # gobjectIntrospection = pkgs.callPackage ./fixes/gobject-introspection {};

    # pango = pkgs.callPackage ./fixes/pango.nix {};

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

    gst_all_1 =
      let super = pkgs.gst_all_1.override { fluidsynth = null; };
      in super // {
        gstreamer = super.gstreamer.overrideDerivation (old: {
          CFLAGS = " -Wno-error ";
          CXXFLAGS = " -Wno-error ";

          configureFlags = [
            "--disable-shared"
            "--enable-static"
          ];

          buildInputs = old.buildInputs ++ [
            pkgs.windows.mingw_w64_pthreads.crossDrv
          ];
        });

        gst-plugins-base = super.gst-plugins-base.overrideDerivation (old: {
          #CFLAGS = " -Wno-error ";
          #CXXFLAGS = " -Wno-error ";

          configureFlags = [
            "--disable-shared"
            "--enable-static"
          ];

          #buildInputs = old.buildInputs ++ [
          #  pkgs.windows.mingw_w64_pthreads.crossDrv
          #];
        });
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

    zeromq4 = let inherit (pkgs.stdenv.lib) overrideDerivation;
                  zmq4 = pkgs.zeromq4.override { libuuid = null; };
              in zmq4 // {
                crossDrv = overrideDerivation zmq4.crossDrv (old: {
                  patches = [
                    ./fixes/zeromq/includes-consistent.patch
                    ./fixes/zeromq/winxp-compatibility.patch
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
          pkgs.mingw-std-threads
          pkgs.windows.mingw_w64_pthreads.crossDrv
        ];
      });
    };

    # python2 = pkgs.python2.overrideDerivation (old: {
    #   prePatch = ''
    #       rm -f Misc/config_mingw \
    #             Misc/cross_mingw32 \
    #             Misc/python-config.sh.in \
    #             Misc/cross_mingw32 \
    #             Misc/python-config-u.sh.in \
    #             Python/fileblocks.c \
    #             Lib/list2cmdline.py
    #   '';
    #  
    #   postPatch = ''
    #       #autoreconf -vfi
    #  
    #       sed -i "/SQLITE_OMIT_LOAD_EXTENSION/d" setup.py
    #  
    #       touch Include/graminit.h
    #       touch Python/graminit.c
    #       touch Parser/Python.asdl
    #       touch Parser/asdl.py
    #       touch Parser/asdl_c.py
    #       touch Include/Python-ast.h
    #       touch Python/Python-ast.c
    #       echo \"\" > Parser/pgen.stamp
    #  
    #       rm -r Modules/expat
    #       rm -r Modules/zlib
    #       rm -r Modules/_ctypes/{darwin,libffi}*
    #  
    #       export CFLAGS+=" -fwrapv -D__USE_MINGW_ANSI_STDIO=1 "
    #       export CXXFLAGS+=" -fwrapv -D__USE_MINGW_ANSI_STDIO=1 "
    #       export CPPFLAGS+=" -I$PREFIX_WIN/include/ncursesw "
    #  
    #       export ac_cv_working_tzset=no
    #   '';
    #  
    #   configureFlags = old.configureFlags ++ [
    #     "--with-system-expat"
    #     "--with-system-ffi"
    #   ];
    #  
    #   patches = [
    #     ./fixes/python2/0000-make-_sysconfigdata.py-relocatable.patch
    #     ./fixes/python2/0001-fix-_nt_quote_args-using-subprocess-list2cmdline.patch
    #     ./fixes/python2/0100-MINGW-BASE-use-NT-thread-model.patch
    #     ./fixes/python2/0110-MINGW-translate-gcc-internal-defines-to-python-platf.patch
    #     ./fixes/python2/0120-MINGW-use-header-in-lowercase.patch
    #     ./fixes/python2/0130-MINGW-configure-MACHDEP-and-platform-for-build.patch
    #     ./fixes/python2/0140-MINGW-preset-configure-defaults.patch
    #     ./fixes/python2/0150-MINGW-configure-largefile-support-for-windows-builds.patch
    #     ./fixes/python2/0160-MINGW-add-wincrypt.h-in-Python-random.c.patch
    #     ./fixes/python2/0180-MINGW-init-system-calls.patch
    #     ./fixes/python2/0190-MINGW-detect-REPARSE_DATA_BUFFER.patch
    #     ./fixes/python2/0200-MINGW-build-in-windows-modules-winreg.patch
    #     ./fixes/python2/0210-MINGW-determine-if-pwdmodule-should-be-used.patch
    #     ./fixes/python2/0220-MINGW-default-sys.path-calculations-for-windows-plat.patch
    #     ./fixes/python2/0230-MINGW-AC_LIBOBJ-replacement-of-fileblocks.patch
    #     ./fixes/python2/0250-MINGW-compiler-customize-mingw-cygwin-compilers.patch
    #     ./fixes/python2/0270-CYGWIN-issue13756-Python-make-fail-on-cygwin.patch
    #     ./fixes/python2/0290-issue6672-v2-Add-Mingw-recognition-to-pyport.h-to-al.patch
    #     ./fixes/python2/0300-MINGW-configure-for-shared-build.patch
    #     ./fixes/python2/0310-MINGW-dynamic-loading-support.patch
    #     ./fixes/python2/0320-MINGW-implement-exec-prefix.patch
    #     ./fixes/python2/0330-MINGW-ignore-main-program-for-frozen-scripts.patch
    #     ./fixes/python2/0340-MINGW-setup-exclude-termios-module.patch
    #     ./fixes/python2/0350-MINGW-setup-_multiprocessing-module.patch
    #     ./fixes/python2/0360-MINGW-setup-select-module.patch
    #     ./fixes/python2/0370-MINGW-setup-_ctypes-module-with-system-libffi.patch
    #     ./fixes/python2/0380-MINGW-defect-winsock2-and-setup-_socket-module.patch
    #     ./fixes/python2/0390-MINGW-exclude-unix-only-modules.patch
    #     ./fixes/python2/0400-MINGW-setup-msvcrt-module.patch
    #     ./fixes/python2/0410-MINGW-build-extensions-with-GCC.patch
    #     ./fixes/python2/0420-MINGW-use-Mingw32CCompiler-as-default-compiler-for-m.patch
    #     ./fixes/python2/0430-MINGW-find-import-library.patch
    #     ./fixes/python2/0440-MINGW-setup-_ssl-module.patch
    #     ./fixes/python2/0460-MINGW-generalization-of-posix-build-in-sysconfig.py.patch
    #     ./fixes/python2/0462-MINGW-support-stdcall-without-underscore.patch
    #     ./fixes/python2/0480-MINGW-generalization-of-posix-build-in-distutils-sys.patch
    #     ./fixes/python2/0490-MINGW-customize-site.patch
    #     ./fixes/python2/0500-add-python-config-sh.patch
    #     ./fixes/python2/0510-cross-darwin-feature.patch
    #     ./fixes/python2/0520-py3k-mingw-ntthreads-vs-pthreads.patch
    #     ./fixes/python2/0530-mingw-system-libffi.patch
    #     ./fixes/python2/0540-mingw-semicolon-DELIM.patch
    #     ./fixes/python2/0550-mingw-regen-use-stddef_h.patch
    #     ./fixes/python2/0560-mingw-use-posix-getpath.patch
    #     ./fixes/python2/0565-mingw-add-ModuleFileName-dir-to-PATH.patch
    #     ./fixes/python2/0570-mingw-add-BUILDIN_WIN32_MODULEs-time-msvcrt.patch
    #     ./fixes/python2/0580-mingw32-test-REPARSE_DATA_BUFFER.patch
    #     ./fixes/python2/0590-mingw-INSTALL_SHARED-LDLIBRARY-LIBPL.patch
    #     ./fixes/python2/0600-msys-mingw-prefer-unix-sep-if-MSYSTEM.patch
    #     ./fixes/python2/0610-msys-cygwin-semi-native-build-sysconfig.patch
    #     ./fixes/python2/0620-mingw-sysconfig-like-posix.patch
    #     ./fixes/python2/0630-mingw-_winapi_as_builtin_for_Popen_in_cygwinccompiler.patch
    #     ./fixes/python2/0640-mingw-x86_64-size_t-format-specifier-pid_t.patch
    #     ./fixes/python2/0650-cross-dont-add-multiarch-paths-if-cross-compiling.patch
    #     ./fixes/python2/0660-mingw-use-backslashes-in-compileall-py.patch
    #     ./fixes/python2/0670-msys-convert_path-fix-and-root-hack.patch
    #     ./fixes/python2/0690-allow-static-tcltk.patch
    #     ./fixes/python2/0710-CROSS-properly-detect-WINDOW-_flags-for-different-nc.patch
    #     ./fixes/python2/0720-mingw-pdcurses_ISPAD.patch
    #     ./fixes/python2/0740-grammar-fixes.patch
    #     ./fixes/python2/0750-Add-interp-Python-DESTSHARED-to-PYTHONPATH-b4-pybuilddir-txt-dir.patch
    #     ./fixes/python2/0760-msys-monkeypatch-os-system-via-sh-exe.patch
    #     ./fixes/python2/0770-msys-replace-slashes-used-in-io-redirection.patch
    #     ./fixes/python2/0790-mingw-add-_exec_prefix-for-tcltk-dlls.patch
    #     ./fixes/python2/0800-mingw-install-layout-as-posix.patch
    #     ./fixes/python2/0820-mingw-reorder-bininstall-ln-symlink-creation.patch
    #     ./fixes/python2/0830-add-build-sysroot-config-option.patch
    #     ./fixes/python2/0840-add-builddir-to-library_dirs.patch
    #     ./fixes/python2/0845-Remove-compiler-lib-dirs-from-extension-lib-dirs.patch
    #     ./fixes/python2/0850-cross-PYTHON_FOR_BUILD-gteq-276-and-fullpath-it.patch
    #     ./fixes/python2/0855-mingw-fix-ssl-dont-use-enum_certificates.patch
    #     ./fixes/python2/0860-mingw-build-optimized-ext.patch
    #     ./fixes/python2/0870-mingw-add-LIBPL-to-library-dirs.patch
    #     ./fixes/python2/0910-fix-using-dllhandle-and-winver-mingw.patch
    #     ./fixes/python2/0970-Add-AMD64-to-sys-config-so-msvccompiler-get_build_version-works.patch
    #     ./fixes/python2/0980-mingw-readline-features-skip.patch
    #     ./fixes/python2/1000-dont-link-with-gettext.patch
    #     ./fixes/python2/1010-ctypes-python-dll.patch
    #     ./fixes/python2/1020-gdbm-module-includes.patch
    #     ./fixes/python2/1030-use-gnu_printf-in-format.patch
    #     ./fixes/python2/1040-install-msilib.patch
    #     ./fixes/python2/1050-skip-add-db-includes-for-win.patch
    #     ./fixes/python2/2000-distutils-add-windmc-to-cygwinccompiler.patch
    #   ];
    # });
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
