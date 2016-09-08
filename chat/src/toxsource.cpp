#include "toxsource.hpp"

#include <QThread>
#include <QDebug>

G_BEGIN_DECLS

struct ToxSourceStreamClass {
	GstElementClass parent_class;
};

#define tox_source_stream_parent_class parent_class
G_DEFINE_TYPE(ToxSourceStream, tox_source_stream, GST_TYPE_ELEMENT)

void tox_source_stream_set_property(GObject *self, guint propid, const GValue *value, GParamSpec *pspec) {
	qDebug() << __func__ << self << propid << value << pspec;
}

void tox_source_stream_get_property(GObject *self, guint propid, GValue *value, GParamSpec *pspec) {
	qDebug() << __func__ << self << propid << value << pspec;
}

GstStateChangeReturn tox_source_stream_change_state(GstElement *self, GstStateChange transition) {
	GstStateChangeReturn ret;

	qDebug() << __func__ << self << transition;

	ret = GST_ELEMENT_CLASS(parent_class)->change_state(self, transition);
	
	qDebug() << ret;
	
	return ret;
}

static gboolean tox_source_stream_send_event(GstElement *self, GstEvent *event) {
	gboolean res;

	res = GST_ELEMENT_CLASS(parent_class)->send_event(self, event);

	return res;
}
enum {
	PROP_0, PROP_STREAM, PROP_N
};

static GstStaticPadTemplate toxtemplate = GST_STATIC_PAD_TEMPLATE ("src", GST_PAD_SRC, GST_PAD_ALWAYS, GST_STATIC_CAPS ("audio/x-raw"));

static GParamSpec *obj_properties[PROP_N] = { nullptr, };

static void tox_source_stream_class_init(ToxSourceStreamClass *klass) {
    qDebug() << __func__ << klass << QThread::currentThread();
    GObjectClass *gobject_class = G_OBJECT_CLASS(klass);
	GstElementClass *gstelement_class = (GstElementClass *) klass;

    obj_properties[PROP_STREAM] = g_param_spec_pointer("call","Call","AudioCall instance",G_PARAM_READWRITE);

    gobject_class->set_property = tox_source_stream_set_property;
    gobject_class->get_property = tox_source_stream_get_property;

    gstelement_class->send_event = tox_source_stream_send_event;
    //gstelement_class->provide_clock = tox_source_stream_provide_clock;
    gstelement_class->change_state = tox_source_stream_change_state;

    gst_element_class_add_static_pad_template(gstelement_class, &toxtemplate);

    g_object_class_install_properties(gobject_class, PROP_N, obj_properties);
}
static void tox_source_stream_init(ToxSourceStream *self) {

}
G_END_DECLS