#pragma once

#include <glib-object.h>
#include <gio/gio.h>

#include <QDebug>

class AudioCall;

G_BEGIN_DECLS

#define TOX_TYPE_OUTPUT (tox_output_stream_get_type())

typedef struct _ToxOutputStream ToxOutputStream;

struct _ToxOutputStream {
    GOutputStream parent_class;
    AudioCall *call;
};

GType tox_output_stream_get_type();

G_END_DECLS
