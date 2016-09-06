#include <iostream>
#include <QCoreApplication>
#include <Qt5GStreamer/QGlib/Error>
#include <Qt5GStreamer/QGlib/Connect>
#include <Qt5GStreamer/QGst/Init>
#include <Qt5GStreamer/QGst/Bus>
#include <Qt5GStreamer/QGst/Pipeline>
#include <Qt5GStreamer/QGst/Parse>
#include <Qt5GStreamer/QGst/Message>
#include <Qt5GStreamer/QGst/Utils/ApplicationSink>
#include <Qt5GStreamer/QGst/Utils/ApplicationSource>

class MySink : public QGst::Utils::ApplicationSink {
public:
    explicit MySink(QGst::Utils::ApplicationSource *src)
        : QGst::Utils::ApplicationSink(), m_src(src) {}

protected:
    virtual void eos() {
        m_src->endOfStream();
    }

    virtual QGst::FlowReturn newSample() {
        QGst::SamplePtr sample = pullSample();
        m_src->pushBuffer(sample->buffer());
        return QGst::FlowOk;
    }

private:
    QGst::Utils::ApplicationSource *m_src;
};

class Player : public QCoreApplication {
public:
    Player(int argc, char **argv);
    ~Player();
private:
    void onBusMessage(const QGst::MessagePtr & message);
private:
    QGst::Utils::ApplicationSource m_src;
    MySink m_sink;
    QGst::PipelinePtr pipeline1;
    QGst::PipelinePtr pipeline2;
};

Player::Player(int argc, char** argv)
    : QCoreApplication(argc, argv), m_sink(&m_src) {
    QGst::init(&argc, &argv);
    const char* caps = "audio/x-opus, channel-mapping-family=(int)0";
    QString pipe1 = QString("audiotestsrc ! opusenc"
                            " ! appsink name=\"a\" caps=\"%2\"").arg(caps);
    pipeline1 = QGst::Parse::launch(pipe1).dynamicCast<QGst::Pipeline>();
    m_sink.setElement(pipeline1->getElementByName("a"));
    QGlib::connect(pipeline1->bus(), "message::error", this,
                   &Player::onBusMessage);
    pipeline1->bus()->addSignalWatch();

    QString pipe2 = QString("appsrc name=\"b\" is-live=true caps=\"%2\" format=3"
                            " ! decodebin ! pulsesink").arg(caps);
    pipeline2 = QGst::Parse::launch(pipe2).dynamicCast<QGst::Pipeline>();
    m_src.setElement(pipeline2->getElementByName("b"));
    QGlib::connect(pipeline2->bus(), "message", this, &Player::onBusMessage);
    pipeline2->bus()->addSignalWatch();

    pipeline1->setState(QGst::StatePlaying);
    pipeline2->setState(QGst::StatePlaying);
}

Player::~Player() {
    pipeline1->setState(QGst::StateNull);
    pipeline2->setState(QGst::StateNull);
}

void Player::onBusMessage(const QGst::MessagePtr& message) {
    switch(message->type()) {
    case QGst::MessageEos: quit(); break;
    case QGst::MessageError:
        qCritical() << message.staticCast<QGst::ErrorMessage>()->error();
        break;
    default: break;
    }
}

int main(int argc, char** argv) {
    Player p(argc, argv);
    return p.exec();
}
