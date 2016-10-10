{ stdenv, fetchFromGitHub, autoreconfHook, libsodium, ncurses, libopus
, libvpx, check, libconfig, pkgconfig, windows }:

stdenv.mkDerivation rec {
  name = "tox-core-dev-20160727";
      
      src = fetchFromGitHub {
        owner = "TokTok";
        repo = "toxcore";
        rev = "05f474b4df8171412237f46c943822edd202b4a9";
        sha256 = "1wq0nbdcq125gcg7pqwqwa0pvh7zg78drd2f585b0a00m1rhzpdy";
      };
      patches = [ ./../../toxcore-expose-udp.patch ];
      propagatedNativeBuildInputs = [];

  NIX_LDFLAGS = "-lgcc_s";

  postPatch = ''
    # within Nix chroot builds, localhost is unresolvable
    sed -i -e '/DEFTESTCASE(addr_resolv_localhost)/d' \
      auto_tests/network_test.c
    # takes WAAAY too long (~10 minutes) and would timeout
    sed -i -e '/DEFTESTCASE[^(]*(many_clients\>/d' \
      auto_tests/tox_test.c
  '';

  configureFlags = [
    "--with-libsodium-headers=${libsodium.dev}/include"
    "--with-libsodium-libs=${libsodium.out}/lib"
    "--enable-ntox"
    "--enable-daemon"
  ];

  nativeBuildInputs = [ autoreconfHook pkgconfig ];
  buildInputs = [
    libsodium ncurses check libconfig
  ] ++ stdenv.lib.optionals (!stdenv.isArm) [
    libopus
  ];

  crossAttrs = {
    buildInputs = map (x: x.crossDrv) (buildInputs ++ [ windows.mingw_w64_pthreads ]);
    configureFlags = [
      "--with-libsodium-headers=${libsodium.crossDrv.dev}/include"
      "--with-libsodium-libs=${libsodium.crossDrv.out}/lib"
      "--disable-ntox"
      "--disable-daemon"
    ];
  };

  propagatedBuildInputs = stdenv.lib.optionals (!stdenv.isArm) [ libvpx ];

  # Some tests fail randomly due to timeout. This kind of problem is well known
  # by upstream: https://github.com/irungentoo/toxcore/issues/{950,1054}
  # They don't recommend running tests on 50core machines with other cpu-bound
  # tests running in parallel.
  #
  # NOTE: run the tests locally on your machine before upgrading this package!
  doCheck = false;

  meta = with stdenv.lib; {
    description = "P2P FOSS instant messaging application aimed to replace Skype with crypto";
    license = licenses.gpl3Plus;
    maintainers = with maintainers; [ viric jgeerds ];
    platforms = platforms.all;
  };
}
