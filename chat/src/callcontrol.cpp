#include "callcontrol.hpp"
#include "core.hpp"
#include "audiocall.hpp"

#include <QVBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QLineEdit>

using namespace chat;

CallControl::CallControl(Core *core, Friend *fr) : core(core), fr(fr), ac(0) {
	QVBoxLayout *layout = new QVBoxLayout(this);
	ac = new AudioCall(core, fr);

	status = new QLabel(this);
	layout->addWidget(status);
	status->setText("initialized");

	QPushButton *but = new QPushButton("start",this);
	layout->addWidget(but);
	connect(but, SIGNAL(clicked()), this, SLOT(start()));

	but = new QPushButton("stop", this);
	layout->addWidget(but);
	connect(but, SIGNAL(clicked()), this, SLOT(stop()));

	outbound = new QLineEdit("pulsesrc ! opusenc ! gdppay ! appsink name=\"toxsink\" caps=\"application/x-gdp\"",this);
	layout->addWidget(outbound);

	inbound = new QLineEdit("appsrc name=\"toxsrc\" caps=\"application/x-gdp\" is-live=true format=3 ! gdpdepay ! opusdec ! pulsesink");
	layout->addWidget(inbound);
}

CallControl::~CallControl() {
	if (ac) {
		if (ac->started) ac->stop();
		delete ac;
	}
}

void CallControl::start() {
	status->setText("starting");
	if (!ac->started) ac->start(inbound->text(), outbound->text());
	status->setText("stopped");
}

void CallControl::packet(QByteArray data) {
	ac->packet(data);
}

void CallControl::stop() {
	status->setText("stopping");
	if (ac->started) ac->stop();
	status->setText("stopped");
}