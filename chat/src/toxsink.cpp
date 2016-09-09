#include "toxsink.hpp"
#include "audiocall.hpp"

namespace chat {
ToxSink::ToxSink() {

}
QGst::FlowReturn ToxSink::newSample() {
	QGst::SamplePtr sample = pullSample();
	QGst::BufferPtr buf = sample->buffer();
	QGst::MapInfo mi;
	buf->map(mi, QGst::MapRead);
	audioCall->write_fn(QByteArray(reinterpret_cast<const char*>(mi.data()), mi.size()));
	buf->unmap(mi);
	return QGst::FlowOk;
}

}