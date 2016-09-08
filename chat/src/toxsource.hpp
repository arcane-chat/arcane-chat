#pragma once

#include <glib-object.h>
#include <gst/gst.h>

G_BEGIN_DECLS

#define TOX_TYPE_SOURCE (tox_source_stream_get_type())

typedef struct _ToxSourceStream ToxSourceStream;

struct _ToxSourceStream {
	GstElement parent;
};

GType tox_source_stream_get_type();

G_END_DECLS