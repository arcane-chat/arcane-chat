#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QModelIndex>

#include "friend.h"

namespace Ui {
class MainWindow;
}

class ChatWidget;
class InfoWidget;
class ChannelModel;
namespace chat {
    class Core;
}

class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit MainWindow(QList<chat::Friend*> friends, chat::Core *core);
    ~MainWindow();

private slots:
    void on_doubleclick(QModelIndex index);
private:
    Ui::MainWindow *ui;
    ChatWidget *chatWidget;
    InfoWidget *infoWidget;
    ChannelModel *model;
};

#endif // MAINWINDOW_H
