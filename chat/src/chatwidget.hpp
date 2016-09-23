#pragma once

#include "utils.hpp"

#include <QWidget>
#include <QTextBrowser>

namespace Ui {
    class ChatWidget;
}

namespace chat {
    class Core;
    class Friend;
}

namespace gui {

class FriendNode;

using namespace chat;

//! widget to manage the state of a single tab in the chat tabstrip
class ChatSection : public QWidget {
    Q_OBJECT
public:
    ChatSection(chat::Friend* f);

    chat::Friend* f;
    QTextEdit* text;

private:
    QTextDocument* doc;
};

//! the entire chat pannel
class ChatWidget : public QWidget {
    Q_OBJECT

public:
    explicit ChatWidget(chat::Core* core, QWidget* parent = 0);
    void open_chat(FriendNode* fn);
    ~ChatWidget();

private:
    Ui::ChatWidget* ui;
    QMap<uint32_t, ChatSection*> chatSections;
    chat::Core* core;

private Q_SLOTS:
    void return_pressed();
    void on_message(Friend* f, bool action, QString message);
};

}
