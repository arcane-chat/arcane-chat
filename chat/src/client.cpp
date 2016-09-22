#include "options.hpp"
#include "core.hpp"
#include "utils.hpp"
#include "friend.hpp"
#include "mainwindow.hpp"
#include "version.hpp"

#include <iostream>
#include <vector>
#include <tox/tox.h>
#include <cassert>
#include <cstdio>
#include <cstring>
#include <signal.h>

#include <giomm/init.h>

#include <QApplication>
#include <QDebug>
#include <QCommandLineParser>
#include <Qt5GStreamer/QGst/Init>

void handler(int /* signum */) {
    qDebug() << "sending qca::quit()\n";
    QCoreApplication::quit();
    qDebug() << "signal handler finished\n";
}

int main(int argc, char** argv) {
    QGst::init(&argc, &argv);
    Gio::init();

    QApplication app(argc, argv);

    app.setApplicationName("arcane-chat-client");
    app.setApplicationVersion(ARCANE_CHAT_VERSION);

    QCommandLineParser parser;

    parser.setApplicationDescription("The client for Arcane Chat.");
    parser.addHelpOption();
    parser.addVersionOption();

    QCommandLineOption sendFriendRequestOption = {
        QStringList() << "r" << "request",
        app.translate("main", "Send a friend request to <toxid>."),
        app.translate("main", "toxid")
    };
    parser.addOption(sendFriendRequestOption);

    QCommandLineOption tracerOption = {
        QStringList() << "t" << "tracer",
        app.translate("main", "Verbosely output a trace of some Qt signals.")
    };
    parser.addOption(tracerOption);

    QCommandLineOption headlessOption = {
        QStringList() << "headless",
        app.translate("main", "Run the client headlessly.")
    };
    parser.addOption(headlessOption);

    parser.process(app);

    // EXAMPLES OF PARSER USE:
    //     https://gist.github.com/taktoa/d8b33c1aa81d97a15fb0dff43ba22b98

    int ret = 1;

    {
        chat::Core core { "/tmp/client/" };

        if(parser.isSet(sendFriendRequestOption)) {
            QByteArray hex;
            hex.append(parser.value(sendFriendRequestOption));
            // TODO: figure out an appropriate message to send
            core.friend_add(QByteArray::fromHex(hex), "placeholder");
        }

        if(parser.isSet(tracerOption)) {
            new Tracer(&core);
        }

        if(!parser.isSet(headlessOption)) {
            gui::MainWindow* mw = new gui::MainWindow(&core);
            mw->show();
        }

        // Set up a signal handler for SIGINT/SIGTERM
        struct sigaction interrupt;
        memset(&interrupt, 0, sizeof(interrupt));
        interrupt.sa_handler = &handler;
        sigaction(SIGINT, &interrupt, nullptr);
        sigaction(SIGTERM, &interrupt, nullptr);

        ret = app.exec();
    }

    qDebug() << "clean shutdown\n";
    return ret;
}
