#include <glib-object.h>
#include <gio/gio.h>

#include <QDebug>

G_BEGIN_DECLS

#define TOX_TYPE_INPUT (tox_input_stream_get_type())

typedef struct _ToxInputStream ToxInputStream;

struct _ToxInputStream {
    GInputStream parent_class;

};

GType tox_input_stream_get_type();

G_END_DECLS
