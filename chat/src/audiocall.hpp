#pragma once

#include "toxsink.hpp"

#include <Qt5GStreamer/QGst/Pipeline>
#include <Qt5GStreamer/QGst/Message>
#include <Qt5GStreamer/QGst/Utils/ApplicationSource>

#include <QObject>

namespace chat {
    class Core;
    class Friend;

//! ui class to manage an audio call
class AudioCall : public QObject
{
    Q_OBJECT
public:
    explicit AudioCall(chat::Core *core, chat::Friend *fr);
    ~AudioCall();
    void packet(QByteArray data);

    ssize_t write_fn(QByteArray data);

    bool started;
Q_SIGNALS:

public Q_SLOTS:
    void start(QString inbound, QString outbound);
    void stop();
    void onBusMessage(const QGst::MessagePtr & message);
private:
    //bool on_bus_message(const Glib::RefPtr<Gst::Bus>&,
    //                    const Glib::RefPtr<Gst::Message>& message);

    chat::Core *core;
    chat::Friend *fr;
    QGst::PipelinePtr outbound;
    QGst::PipelinePtr inbound;
    chat::ToxSink m_sink;
    QGst::Utils::ApplicationSource m_src;
};
}