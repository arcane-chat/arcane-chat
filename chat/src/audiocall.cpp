#include <iostream>
#include <giomm/init.h>

#include "audiocall.hpp"
#include "core.hpp"
#include <QThread>
#include <Qt5GStreamer/QGst/Parse>
#include <Qt5GStreamer/QGlib/Error>
#include <Qt5GStreamer/QGlib/Connect>
#include <Qt5GStreamer/QGst/Bus>

namespace {
class GstException : public std::exception {
private:
    std::string description_;

public:
    explicit GstException(std::string desc) : description_(desc) {}

    virtual const char* what() const throw() {
        return description_.c_str();
    }
};
}

AudioCall::AudioCall(chat::Core *core, chat::Friend *fr)
    : QObject(core), /*outputstream(nullptr),*/ core(core), fr(fr) {}

/*bool AudioCall::on_bus_message(const ref<Gst::Bus>&, const ref<Gst::Message>& message) {
  switch(message->get_message_type()) {
  case Gst::MESSAGE_EOS:
      std::cerr << std::endl << "End of stream" << std::endl;
      return false;
  case Gst::MESSAGE_STATE_CHANGED:
      std::cerr << "State."
                << (ref<Gst::MessageStateChanged>::cast_static(message)->parse_old())
                << " -> "
                << (ref<Gst::MessageStateChanged>::cast_static(message)->parse())
                << std::endl;
      return true;
  case Gst::MESSAGE_INFO:
      std::cerr << "Info."
                << (ref<Gst::MessageInfo>::cast_static(message)->parse_debug())
                << std::endl;
      return true;
  case Gst::MESSAGE_WARNING:
      std::cerr << "Warning."
                << (ref<Gst::MessageWarning>::cast_static(message)->parse_debug())
                << std::endl;
      return true;
  case Gst::MESSAGE_ERROR:
      std::cerr << "Error."
                << (ref<Gst::MessageError>::cast_static(message)->parse_debug())
                << std::endl;
      return false;
  default: break;
  }

  return true;
}*/


AudioCall::~AudioCall() {
  stop();
}
void AudioCall::stop() {
    qDebug() << __func__;
    outbound->setState(QGst::StateNull);
    inbound->setState(QGst::StateNull);
    core->call_stop(fr);
}

void AudioCall::packet(QByteArray data) {
  QGst::BufferPtr buf = QGst::Buffer::create(data.size());
  QGst::MapInfo mi;
  buf->map(mi, QGst::MapWrite);
  memcpy(mi.data(), data.data(), data.size());
  buf->unmap(mi);
  m_src.pushBuffer(buf);
}
ssize_t AudioCall::write_fn(QByteArray data) {
    core->call_data(fr, data);
    return data.size();
}

void AudioCall::start() {
  //const char *caps = "audio/x-raw, format=(string)S16LE, channels=(int)1, rate=(int)48000, layout=(string)interleaved";
  const char *caps = "application/x-gdp";

  QString outbound_pipeline = QString("pulsesrc ! opusenc ! gdppay ! appsink name=\"toxsink\" caps=\"%1\"").arg(caps);
  qDebug() << outbound_pipeline;
  outbound = QGst::Parse::launch(outbound_pipeline).dynamicCast<QGst::Pipeline>();
  m_sink.audioCall = this;
  m_sink.setElement(outbound->getElementByName("toxsink"));
  //m_sink.setBlockSize(1000);
  QGlib::connect(outbound->bus(), "message::error", this, &AudioCall::onBusMessage);
  outbound->bus()->addSignalWatch();

  QString inbound_pipeline = QString("appsrc name=\"toxsrc\" caps=\"%1\" is-live=true format=3 ! gdpdepay ! opusdec ! pulsesink").arg(caps);
  inbound = QGst::Parse::launch(inbound_pipeline).dynamicCast<QGst::Pipeline>();
  m_src.setElement(inbound->getElementByName("toxsrc"));
  QGlib::connect(inbound->bus(), "message::error", this, &AudioCall::onBusMessage);
  inbound->bus()->addSignalWatch();

  outbound->setState(QGst::StatePlaying);
  inbound->setState(QGst::StatePlaying);
  core->call_start(fr);
}

void AudioCall::onBusMessage(const QGst::MessagePtr & message) {
    switch (message->type()) {
  case QGst::MessageEos:
      qDebug() << "end of stream";
      break;
  case QGst::MessageError:
      qCritical() << message.staticCast<QGst::ErrorMessage>()->error();
      break;
  default:
      break;
  }
}