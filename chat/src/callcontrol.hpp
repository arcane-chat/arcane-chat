#pragma once

#include <QWidget>
#include <QByteArray>

class QLabel;
class QLineEdit;

namespace chat {
class Core;
class Friend;
class AudioCall;

class CallControl : public QWidget {
Q_OBJECT
public:
	explicit CallControl(Core *core, Friend *fr);
	~CallControl();
public Q_SLOTS:
	void start();
	void packet(QByteArray data);
	void stop();
private:
	chat::Core *core;
	chat::Friend *fr;
	QLabel *status;
	AudioCall *ac;
	QLineEdit *inbound, *outbound;
};
}