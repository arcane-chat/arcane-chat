#pragma once

#include <QMainWindow>
#include <QModelIndex>

#include "friend.hpp"

namespace Ui {
class MainWindow;
}

namespace chat {
class Core;
}

namespace gui {
class ChatWidget;
class InfoWidget;
class ChannelModel;

class MainWindow : public QMainWindow {
    Q_OBJECT

public:
    explicit MainWindow(chat::Core* core);
    ~MainWindow();

private Q_SLOTS:
    void on_doubleclick(QModelIndex index);

private:
    Ui::MainWindow* ui;
    ChatWidget* chatWidget;
    InfoWidget* infoWidget;
    ChannelModel* model;
};
}