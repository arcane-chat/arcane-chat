#ifndef CHATWIDGET_H
#define CHATWIDGET_H

#include <QWidget>
#include <QTextBrowser>
#include "utils.hpp"

namespace Ui {
    class ChatWidget;
}

class FriendNode;
namespace chat {
    class Core;
    class Friend;
}

using namespace chat;

class ChatSection : public QWidget {
    Q_OBJECT
public:
    ChatSection(chat::Friend* f);

    chat::Friend* f;
    QTextEdit* text;

private:
    QTextDocument* doc;
};

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

private slots:
    void return_pressed();
    void on_message(Friend* f, bool action, QString message);
};

#endif // CHATWIDGET_H
