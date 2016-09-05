#include <iostream>
#include <opus/opus.h>
#include <math.h>
#include <QDebug>
#include <pulse/simple.h>
#include <unistd.h>
#include <cassert>

#include "pulseaudio_loop.hpp"

using namespace std;

#define FREQ 15000

int pulse_test(opus_int16 *samples, int count) {
    pa_simple *s;
    pa_sample_spec ss;
    ss.format = PA_SAMPLE_S16NE;
    ss.channels = 1;
    ss.rate = 48000;

    s = pa_simple_new(nullptr, // server
                      "fuspr-chat", // app name
                      PA_STREAM_PLAYBACK,
                      nullptr, // device
                      "Tone", // description
                      &ss,
                      nullptr,
                      nullptr,
                      nullptr);
    assert(s);
    pa_simple_write(s, samples, count, nullptr);
    pa_simple_write(s, samples, count, nullptr);
    pa_simple_drain(s, nullptr);
    //sleep(6);
    pa_simple_free(s);
    return 0;
}

int opus_main() {

    int samplerate = 48000;
    opus_int16 samples[samplerate];
    for (int i=0; i<samplerate; i++) {
        samples[i] = sin((2 * (float)i * FREQ * 3.1415) / samplerate) * (1 << 10);
        //qDebug() << samples[i];
    }
    //qDebug() << QByteArray((char*)samples,samplerate).toHex();
    //pulse_test(samples,samplerate);
    PAThreadedMainLoop *loop = pa_test_init();
    sleep(5);
    pa_test_write(loop, samples, samplerate);
    sleep(5);
    pa_test_write(loop, samples, samplerate);
    sleep(5);
    pa_test_write(loop, samples, samplerate);
    sleep(5);
    pa_test_stop(loop);

    return 0;

    OpusEncoder* enc = opus_encoder_create(samplerate, 1, OPUS_APPLICATION_VOIP, nullptr);
    qDebug() << enc;

    int framesize = 2880;
    qDebug() << "sending" << ((float)framesize / samplerate) << "seconds";
    int remaining = samplerate;
    int offset = 0;
    while (remaining > framesize) {
        uint8_t frame[4000];
        int used = opus_encode(enc, samples + offset, framesize, frame, 4000);
        qDebug() << used << QByteArray((char*)frame,used).toHex();
        remaining -= framesize;
        offset += framesize;
    }

    cout << opus_get_version_string() << "\n";
    opus_encoder_destroy(enc); enc = 0;
    return 0;
}
