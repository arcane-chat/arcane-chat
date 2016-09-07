#pragma once

#define QT_NO_SIGNALS_SLOTS_KEYWORDS

#include <glibmm.h>
#include <giomm/outputstream.h>
#include <gstreamermm.h>

#include <QObject>
#include "toxoutputstream.hpp"

namespace chat {
    class Core;
    class Friend;
}

class AudioCall : public QObject
{
    Q_OBJECT
public:
    explicit AudioCall(chat::Core *core, chat::Friend *fr);
    void create_instance();
    void create_pipeline();
    void stop_everything();
    ssize_t write_fn(QByteArray data);
Q_SIGNALS:

public Q_SLOTS:
private:
    ToxOutputStream *outputstream;
    Glib::RefPtr<Gio::OutputStream> reference;
    chat::Core *core;
    chat::Friend *fr;
    Glib::RefPtr<Gst::Pipeline> pipeline;
};
