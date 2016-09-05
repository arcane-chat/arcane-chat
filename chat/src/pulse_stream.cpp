#include <pulse/stream.h>

#include "pulse_stream.h"
#include "pulseaudio_loop.hpp"

namespace pulse {

    Stream::Stream(PAThreadedMainLoop* parent) : QObject(parent), loop(parent) {
        pa_sample_spec ss;
        ss.format = PA_SAMPLE_S16NE;
        ss.channels = 1;
        ss.rate = 48000;

        pa_proplist* props = pa_proplist_new();
        stream = pa_stream_new_with_proplist(loop->context, "sample stream",
                                             &ss, nullptr, props);
        pa_proplist_free(props);
        pa_stream_connect_playback(stream, nullptr, nullptr, PA_STREAM_NOFLAGS,
                                   nullptr, nullptr);
    }

    Stream::~Stream() { pa_stream_unref(stream); }

    void Stream::write(int16_t* samples, int count) {
        pa_threaded_mainloop_lock(loop->loop);
        pa_stream_write(stream, samples, count * 2, nullptr, 0,
                        PA_SEEK_RELATIVE);
        pa_threaded_mainloop_unlock(loop->loop);
    }

} // namespace pulse
