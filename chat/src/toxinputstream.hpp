#include <glib-object.h>
#include <gio/gio.h>

#include <QDebug>

G_BEGIN_DECLS

#define TOX_TYPE_INPUT (tox_input_stream_get_type())

typedef struct ToxInputStream {
    GInputStream parent_class;
} ToxInputStream;

GType tox_input_stream_get_type();

G_END_DECLS
