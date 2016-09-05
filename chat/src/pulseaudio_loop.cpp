#include <cassert>
#include <QString>
#include <QDebug>

#include "pulseaudio_loop.hpp"

void pa_state_callback(pa_context *c, void *userdata) {
    PAThreadedMainLoop *x = (PAThreadedMainLoop*)userdata;
    x->state_change_callback();
}

PAThreadedMainLoop::PAThreadedMainLoop() {
    loop = pa_threaded_mainloop_new();
    assert(loop);

    pa_proplist *props = pa_proplist_new();
    set_prop(props,PA_PROP_MEDIA_NAME,"pa test source");
    set_prop(props, PA_PROP_APPLICATION_NAME, "test app");

    context = pa_context_new_with_proplist(pa_threaded_mainloop_get_api(loop),"foo",props);

    pa_proplist_free(props);

    pa_context_set_state_callback(context, pa_state_callback, this);

    pa_context_connect(context, nullptr, PA_CONTEXT_NOFLAGS, nullptr);

    pa_threaded_mainloop_start(loop);
}

void PAThreadedMainLoop::open_stream() {
    pa_sample_spec ss;
    ss.format = PA_SAMPLE_S16NE;
    ss.channels = 1;
    ss.rate = 48000;

    pa_proplist *props = pa_proplist_new();
    stream = pa_stream_new_with_proplist(context, "sample stream", &ss, nullptr, props);
    pa_proplist_free(props);
    pa_stream_connect_playback(stream, nullptr, nullptr, PA_STREAM_NOFLAGS, nullptr, nullptr);
}

int PAThreadedMainLoop::set_prop(pa_proplist *p, const char *key, QString value) {
    QByteArray bytes = value.toUtf8();
    bytes = bytes.append('\0');
    return pa_proplist_sets(p, key, bytes.data());
}

PAThreadedMainLoop::~PAThreadedMainLoop() {
    pa_context_disconnect(context);
    pa_context_unref(context);
    pa_stream_unref(stream);
    pa_threaded_mainloop_stop(loop);
    pa_threaded_mainloop_free(loop);
}

PAThreadedMainLoop *pa_test_init() {
    PAThreadedMainLoop *loop = new PAThreadedMainLoop();
    return loop;
}

void pa_test_stop(PAThreadedMainLoop *loop) {
    delete loop;
}

void pa_test_write(PAThreadedMainLoop *loop, int16_t *samples, int count) {
    loop->write(samples,count);
}

void PAThreadedMainLoop::write(int16_t *samples, int count) {
    pa_threaded_mainloop_lock(loop);
    pa_stream_write(stream, samples, count * 2, nullptr, 0, PA_SEEK_RELATIVE);
    pa_threaded_mainloop_unlock(loop);
}

static QString decode_state(pa_context_state state) {
    switch (state) {
    case PA_CONTEXT_CONNECTING: return "connecting";
    case PA_CONTEXT_AUTHORIZING: return "authorizing";
    case PA_CONTEXT_SETTING_NAME: return "setting name";
    case PA_CONTEXT_READY: return "ready";
    case PA_CONTEXT_TERMINATED: return "terminated";
    default: return QString("unknown %1").arg((int)state);
    }
}

void PAThreadedMainLoop::state_change_callback() {
    // gets ran in the pulseaudio thread
    pa_context_state state = pa_context_get_state(context);

    qDebug() << "state" << decode_state(state);

    if (state == PA_CONTEXT_READY) open_stream();
}
