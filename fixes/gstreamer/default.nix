{ callPackage, libva-full, boost155 }:

rec {
  gstreamer = callPackage ./core {};

  gstreamermm = callPackage ./gstreamermm { inherit gst-plugins-base; };

  gst-plugins-base = callPackage ./base { inherit gstreamer; };

  gst-plugins-good = callPackage ./good { inherit gst-plugins-base; };

  gst-plugins-bad = callPackage ./bad { inherit gst-plugins-base; };

  gst-plugins-ugly = callPackage ./ugly { inherit gst-plugins-base; };

  gst-libav = callPackage ./libav { inherit gst-plugins-base; };

  gst-vaapi = callPackage ./vaapi {
    inherit gst-plugins-base gstreamer gst-plugins-bad;
    libva = libva-full; # looks also for libva-{x11,wayland}
  };

  qt-gstreamer = callPackage ./qt-gstreamer { boost = boost155; };
}
