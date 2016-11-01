#include "audiocall.hpp"
#include "chatwidget.hpp"
#include "channelmodel.hpp"
#include "core.hpp"
#include "config.hpp"

#include <QDebug>
#include <QThread>

#include "ui_chatwidget.h"

#include <Qt5GStreamer/QGst/Parse>
#include <Qt5GStreamer/QGlib/Error>
#include <Qt5GStreamer/QGlib/Connect>
#include <Qt5GStreamer/QGst/Bus>

#include <sstream>

using namespace gui;

ChatWidget::ChatWidget(Core* core, QWidget* parent)
    : QWidget(parent), ui(new Ui::ChatWidget), core(core) {
    ui->setupUi(this);
    connect(ui->lineEdit, &QLineEdit::returnPressed,
            this, &ChatWidget::return_pressed);
    connect(core, &Core::on_message,
            this, &ChatWidget::on_message);
}

ChatWidget::~ChatWidget() { delete ui; }

void ChatWidget::open_chat(FriendNode* fn) {
    auto i = chatSections.find(fn->f->friend_number);
    if(i != chatSections.end()) {
        ChatSection* cs = *i;
        ui->tabWidget->setCurrentWidget(cs);
    } else {
        ChatSection* cs = new ChatSection(fn->f);
        ui->tabWidget->addTab(cs, fn->f->get_username());
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
        ui->tabWidget->addTab(cs, f->get_username());
        chatSections.insert(f->friend_number, cs);
    }
    cs->text->append(QStringLiteral("&lt;%1&gt; %2").arg(f->get_username()).arg(message));
}

static void video_test() {
  QString outbound_pipeline = QStringLiteral("playbin uri=file:///tmp/foo.mkv");
  qDebug() << outbound_pipeline;
  auto outbound = QGst::Parse::launch(outbound_pipeline).dynamicCast<QGst::Pipeline>();
  //m_sink.setBlockSize(1000);
  outbound->setState(QGst::StatePlaying);
}

void ChatWidget::return_pressed() {
    auto msg = ui->lineEdit->text();
    qDebug() << msg;
    ChatSection* cs = reinterpret_cast<ChatSection*>(ui->tabWidget->currentWidget());
    if (CSS_DEBUG && (msg == "/tab")) {
        ChatSection* cs = new ChatSection(nullptr);
        std::ostringstream ss;
        ss << "tab" << rand();
        ui->tabWidget->addTab(cs, QString::fromStdString(ss.str()));
        ui->tabWidget->setCurrentWidget(cs);
    } else if (cs) {
        cs->text->append(QStringLiteral("&lt;%1&gt; %2").arg(core->username).arg(msg));
        if (cs->f) {
            core->send_message(cs->f->friend_number, false, msg);
            if (msg == "/call") {
                qDebug() << "/call was triggered";
                core->open_call_control(cs->f);
            }
        }
    } else {
        QStringList words = msg.split(" ");
        if (msg == "/foo") {
            video_test();
        } else if (words[0] == "/add") {
            QByteArray one = qPrintable(words[1]);
            QByteArray two = QByteArray::fromHex(one);
            core->friend_add(two,"request");
        } else if (words[0] == "/nick") {
            core->set_username(words[1]);
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
