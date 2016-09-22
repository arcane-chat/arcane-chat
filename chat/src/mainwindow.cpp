#include "mainwindow.hpp"
#include "chatwidget.hpp"
#include "infowidget.hpp"
#include "channelmodel.hpp"
#include "channel.hpp"

#include "ui_mainwindow.h"

#include <QDockWidget>
#include <QDebug>

using namespace gui;

MainWindow::MainWindow(chat::Core* core) : core_(core), ui(new Ui::MainWindow) {
    ui->setupUi(this);
    qDebug() << ui->actionCreateChannel->objectName();

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

    model = new ChannelModel(core);
    ui->treeView->setModel(model);

    connect(ui->treeView, &QTreeView::doubleClicked,
        this, &MainWindow::on_doubleclick);
    //connect(ui->actionCreateChannel, SIGNAL(trigger()),
    //    this, SLOT(on_create_channel()));
}

MainWindow::~MainWindow() { delete ui; }

void MainWindow::on_doubleclick(QModelIndex index) {
    Node* node = model->getNode(index);
    qDebug() << "item clicked!" << index << node;
    switch (node->type) {
    case NodeType::LegacyFriend: {
        FriendNode* fn = reinterpret_cast<FriendNode*>(node);
        chatWidget->open_chat(fn);
        break; }
    case NodeType::ChannelNode: {
        ChannelNode *cn = reinterpret_cast<ChannelNode*>(node);
        core_->join_channel(cn->channel());
        break; }
    }
}

void MainWindow::on_actionCreateChannel_triggered() {
    chat::Channel *chan = chat::Channel::create_new();
    chan->set_name("new channel");
    core_->add_owned_channel(chan);
}
