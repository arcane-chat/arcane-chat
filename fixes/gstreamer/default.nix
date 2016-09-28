{ callPackage, qt56, libva-full, boost155, fluidsynth }:

rec {
  gstreamer = callPackage ./core {};

  gstreamermm = callPackage ./gstreamermm { inherit gst-plugins-base; };

  gst-plugins-base = callPackage ./base { inherit gstreamer; };

  gst-plugins-good = callPackage ./good { inherit gst-plugins-base; };

  gst-plugins-bad = callPackage ./bad { inherit gst-plugins-base fluidsynth; };

  gst-plugins-ugly = callPackage ./ugly { inherit gst-plugins-base; };

  gst-libav = callPackage ./libav { inherit gst-plugins-base; };

  gst-vaapi = callPackage ./vaapi {
    inherit gst-plugins-base gstreamer gst-plugins-bad;
    libva = libva-full; # looks also for libva-{x11,wayland}
  };

  qt-gstreamer = qt56.callPackage ./qt-gstreamer {
    inherit gstreamer gst-plugins-base;
    boost = boost155;
  };
}
