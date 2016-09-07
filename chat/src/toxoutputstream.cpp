#include <glibmm.h>

#include "toxoutputstream.hpp"

#include "audiocall.hpp"

#include <QDebug>
#include <QThread>

G_BEGIN_DECLS

struct ToxOutputStreamClass {
    GOutputStreamClass parent_class;
};

static gssize tox_write_fn(GOutputStream* stream,
                           const void* buffer,
                           gsize count,
                           GCancellable* cancellable,
                           GError** error) {
    ToxOutputStream *stream2 = reinterpret_cast<ToxOutputStream*>(stream);
    qDebug() << __func__ << stream << ": " << buffer << count << QThread::currentThread() << QByteArray((char*)buffer,60).toHex();;

    return stream2->call->write_fn(QByteArray(static_cast<const char*>(buffer),count));
    //int retval = g_output_stream_write(stream, buffer, count, cancellable, error);
    //qDebug() << retval << (*error)->message;
    //return count;
}

static gssize tox_splice(GOutputStream* stream,
                         GInputStream* source,
                         GOutputStreamSpliceFlags flags,
                         GCancellable* cancellable,
                         GError** error) {
    qDebug() << __func__ << stream << QThread::currentThread();
    return g_output_stream_splice(stream, source, flags, cancellable, error);
}

static gboolean tox_flush(GOutputStream* stream,
                          GCancellable* cancellable,
                          GError** error) {
    qDebug() << __func__ << stream << QThread::currentThread();
    return g_output_stream_flush(stream, cancellable, error);
}

static gboolean tox_close_fn(GOutputStream* stream,
                             GCancellable* cancellable,
                             GError** error) {
    qDebug() << __func__ << stream << QThread::currentThread();
    return g_output_stream_close(stream, cancellable, error);
}

#define tox_output_stream_parent_class parent_class
G_DEFINE_TYPE(ToxOutputStream, tox_output_stream, G_TYPE_OUTPUT_STREAM);

static void tox_output_stream_class_init(ToxOutputStreamClass* klass) {
    qDebug() << __func__ << klass << QThread::currentThread();
    klass->parent_class.write_fn = tox_write_fn;
    klass->parent_class.splice   = tox_splice;
    klass->parent_class.flush    = tox_flush;
    klass->parent_class.close_fn = tox_close_fn;
}

static void tox_output_stream_init(ToxOutputStream* self) {
    qDebug() << __func__ << self << QThread::currentThread();
}

G_END_DECLS
