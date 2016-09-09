#include "audiocall.hpp"
#include <QDebug>
#include <QThread>

#include "chatwidget.hpp"
#include "channelmodel.hpp"
#include "core.hpp"

#include "ui_chatwidget.h"

#include <Qt5GStreamer/QGst/Parse>
#include <Qt5GStreamer/QGlib/Error>
#include <Qt5GStreamer/QGlib/Connect>
#include <Qt5GStreamer/QGst/Bus>

ChatWidget::ChatWidget(Core* core, QWidget* parent)
    : QWidget(parent), ui(new Ui::ChatWidget), core(core) {
    ui->setupUi(this);
    connect(ui->lineEdit, SIGNAL(returnPressed()), this,
            SLOT(return_pressed()));
    connect(core, SIGNAL(on_message(Friend*, bool, QString)), this,
            SLOT(on_message(Friend*, bool, QString)));
}

ChatWidget::~ChatWidget() { delete ui; }

void ChatWidget::open_chat(FriendNode* fn) {
    auto i = chatSections.find(fn->f->friend_number);
    if(i != chatSections.end()) {
        ChatSection* cs = *i;
        ui->tabWidget->setCurrentWidget(cs);
    } else {
        ChatSection* cs = new ChatSection(fn->f);
        ui->tabWidget->addTab(cs, fn->f->name);
        ui->tabWidget->setCurrentWidget(cs);
        chatSections.insert(fn->f->friend_number, cs);
    }
}

void ChatWidget::on_message(Friend* f, bool action, QString message) {
    auto i = chatSections.find(f->friend_number);
    ChatSection* cs = nullptr;
    if(i != chatSections.end()) {
        cs = *i;
    } else {
        cs = new ChatSection(f);
        ui->tabWidget->addTab(cs, f->name);
        chatSections.insert(f->friend_number, cs);
    }
    cs->text->append(QString("&lt;%1&gt; %2").arg(f->name).arg(message));
}

void video_test() {
  QString outbound_pipeline = QString("playbin uri=file:///tmp/foo.mkv");
  qDebug() << outbound_pipeline;
  auto outbound = QGst::Parse::launch(outbound_pipeline).dynamicCast<QGst::Pipeline>();
  //m_sink.setBlockSize(1000);
  outbound->setState(QGst::StatePlaying);
}
void ChatWidget::return_pressed() {
    auto msg = ui->lineEdit->text();
    qDebug() << msg;
    ChatSection* cs = reinterpret_cast<ChatSection*>(ui->tabWidget->currentWidget());
    if (cs) {
        cs->text->append(QString("&lt;%1&gt; %2").arg(core->username).arg(msg));
        core->send_message(cs->f->friend_number, false, msg);
        if (msg == "!call") {
            qDebug() << "!call was triggered";
            core->call_control(0x01,cs->f,QByteArray());
        }
    } else {
        if (msg == "!foo") {
            video_test();
        } else {
            QByteArray one = qPrintable(msg);
            QByteArray two = QByteArray::fromHex(one);
            qDebug() << two;
            core->friend_add(two,"request");
        }
    }
    ui->lineEdit->setText("");
}

ChatSection::ChatSection(chat::Friend* f) : f(f) {
    auto layout = new QVBoxLayout(this);
    // doc = new QTextDocument(this);
    text = new QTextEdit(this);
    text->setReadOnly(true);
    // text->setDocument(doc);
    layout->addWidget(text);
}
