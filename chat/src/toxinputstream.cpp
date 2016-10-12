#include "toxinputstream.hpp"

#include <QDebug>
#include <QThread>

G_BEGIN_DECLS

struct ToxInputStreamClass {
    GInputStreamClass parent_class;
};

static gssize tox_read_fn(GInputStream* stream,
                          void* buffer,
                          gsize count,
                          GCancellable* cancellable,
                          GError** error) {
    qDebug() << __func__ << stream << QThread::currentThread();
    return count;
}

static gssize tox_skip(GInputStream* stream,
                       gsize count,
                       GCancellable* cancellable,
                       GError** error) {
    qDebug() << __func__ << stream << QThread::currentThread();
    uint8_t buf[count];
    return tox_read_fn(stream, buf, count, cancellable, error);
}

static gboolean tox_close_fn(GInputStream* stream,
                             GCancellable* cancellable,
                             GError** error) {
    qDebug() << __func__ << stream << QThread::currentThread();
    return true;
}

#define tox_input_stream_parent_class parent_class
G_DEFINE_TYPE(ToxInputStream, tox_input_stream, G_TYPE_INPUT_STREAM);

static void tox_input_stream_class_init(ToxInputStreamClass* klass) {
    qDebug() << __func__ << klass << QThread::currentThread();
    klass->parent_class.read_fn  = tox_read_fn;
    klass->parent_class.skip     = tox_skip;
    klass->parent_class.close_fn = tox_close_fn;
}

static void tox_input_stream_init(ToxInputStream* self) {
    qDebug() << __func__ << self << QThread::currentThread();
}

G_END_DECLS
