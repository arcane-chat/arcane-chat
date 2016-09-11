#pragma once

#include <Qt5GStreamer/QGlib/Error>
#include <Qt5GStreamer/QGst/Utils/ApplicationSink>
#include <Qt5GStreamer/QGst/Buffer>


namespace chat {
class AudioCall;

class ToxSink : public QObject, public QGst::Utils::ApplicationSink {
Q_OBJECT
public:
	explicit ToxSink();
	virtual QGst::FlowReturn newSample();

	AudioCall *audioCall;
};
}