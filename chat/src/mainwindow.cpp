#include <QDockWidget>
#include <QDebug>

#include "mainwindow.h"
#include "ui_mainwindow.h"
#include "chatwidget.h"
#include "infowidget.h"
#include "channelmodel.h"

MainWindow::MainWindow(QList<chat::Friend*> friends, chat::Core* core)
    : ui(new Ui::MainWindow) {
    ui->setupUi(this);

    QDockWidget* dock = new QDockWidget(tr("Chat"), this);
    dock->setAllowedAreas(Qt::LeftDockWidgetArea | Qt::RightDockWidgetArea |
                          Qt::BottomDockWidgetArea);
    chatWidget = new ChatWidget(core, dock);
    dock->setWidget(chatWidget);
    addDockWidget(Qt::BottomDockWidgetArea, dock);
    ui->menuView->addAction(dock->toggleViewAction());

    dock = new QDockWidget(tr("Info"), this);
    dock->setAllowedAreas(Qt::LeftDockWidgetArea | Qt::RightDockWidgetArea |
                          Qt::BottomDockWidgetArea);
    infoWidget = new InfoWidget(dock);
    dock->setWidget(infoWidget);
    addDockWidget(Qt::RightDockWidgetArea, dock);
    ui->menuView->addAction(dock->toggleViewAction());

    model = new ChannelModel(friends);
    ui->treeView->setModel(model);

    connect(ui->treeView, SIGNAL(doubleClicked(QModelIndex)), this,
            SLOT(on_doubleclick(QModelIndex)));
}

MainWindow::~MainWindow() { delete ui; }

void MainWindow::on_doubleclick(QModelIndex index) {
    Node* node = model->getNode(index);
    qDebug() << "item clicked!" << index << node;
    if(node->type == NodeType::LegacyFriend) {
        FriendNode* fn = reinterpret_cast<FriendNode*>(node);
        chatWidget->open_chat(fn);
    }
}
