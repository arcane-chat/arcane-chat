{ stdenv, fetchurl, pkgconfig, gst-plugins-base, bzip2, libva, wayland
, libdrm, udev, xorg, mesa, yasm, gstreamer, gst-plugins-bad, nasm
, libvpx, python33
}:

stdenv.mkDerivation rec {
  name = "gst-vaapi-${version}";
  version = "1.9.2";

  src = fetchurl {
    url = "https://gstreamer.freedesktop.org/src/gstreamer-vaapi/gstreamer-vaapi-${version}.tar.xz";
    sha256 = "1qgh71bijg6ij5f706skjqzpix62wdh8dxbjyyykl13rpjlfrjqf";
  };

  outputs = [ "dev" "out" ];

  nativeBuildInputs = with stdenv.lib; [ pkgconfig bzip2 ];

  buildInputs = [
    gstreamer gst-plugins-base gst-plugins-bad libva wayland libdrm udev
    xorg.libX11 xorg.libXext xorg.libXv xorg.libXrandr xorg.libSM xorg.libICE
    mesa nasm libvpx python33
  ];

  preConfigure = "
    export GST_PLUGIN_PATH_1_0=$out/lib/gstreamer-1.0
    mkdir -p $GST_PLUGIN_PATH_1_0
    ";
  configureFlags = "--disable-builtin-libvpx --with-gstreamer-api=1.0";

  meta = {
    homepage = "http://www.freedesktop.org";
    license = stdenv.lib.licenses.lgpl21Plus;
    platforms = stdenv.lib.platforms.linux;
    maintainers = with stdenv.lib.maintainers; [ tstrobel ];
  };
}
