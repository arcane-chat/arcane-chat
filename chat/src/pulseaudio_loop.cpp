#include <cassert>
#include <QString>
#include <QDebug>

#include "pulseaudio_loop.hpp"
#include "pulse_stream.h"

using namespace pulse;

void pa_state_callback(pa_context *, void *userdata) {
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
    playback = new Stream(this);
}

int PAThreadedMainLoop::set_prop(pa_proplist *p, const char *key, QString value) {
    QByteArray bytes = value.toUtf8();
    bytes = bytes.append('\0');
    return pa_proplist_sets(p, key, bytes.data());
}

PAThreadedMainLoop::~PAThreadedMainLoop() {
    pa_context_disconnect(context);
    pa_context_unref(context);
    pa_threaded_mainloop_stop(loop);
    pa_threaded_mainloop_free(loop);
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
