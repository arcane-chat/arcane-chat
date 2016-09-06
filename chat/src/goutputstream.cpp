#include "goutputstream.hpp"

#include <QDebug>

G_BEGIN_DECLS

struct ToxOutputStreamClass {
    GOutputStreamClass parent_class;
};

static gssize tox_write_fn(GOutputStream* stream,
                           const void* buffer,
                           gsize count,
                           GCancellable* cancellable,
                           GError** error) {
    qDebug() << "writing" << buffer << count;
    return count;
}

#define tox_output_stream_parent_class parent_class
G_DEFINE_TYPE(ToxOutputStream, tox_output_stream, G_TYPE_OUTPUT_STREAM);

static void tox_output_stream_class_init(ToxOutputStreamClass* klass) {
    qDebug() << __func__;
    klass->parent_class.write_fn = tox_write_fn;
}

static void tox_output_stream_init(ToxOutputStream* self) {
    qDebug() << __func__;
}

G_END_DECLS
