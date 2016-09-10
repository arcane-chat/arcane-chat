#include <iostream>
#include <boost/algorithm/hex.hpp>
#include <vector>
#include <tox/tox.h>
#include <cassert>
#include <cstdio>
#include <cstring>
#include <signal.h>

#include <giomm/init.h>
#include <gstreamermm.h>
#include <glibmm.h>

#include <QApplication>
#include <QDebug>
#include <QCommandLineParser>
#include <Qt5GStreamer/QGst/Init>

#include "options.hpp"
#include "core.hpp"
#include "utils.hpp"
#include "friend.hpp"
#include "mainwindow.hpp"
#include "version.hpp"

void handler(int signum) {
    std::cout << "Quitting...\n";
    QCoreApplication::quit();
}

int main(int argc, char** argv) {
    QGst::init(&argc, &argv);
    Gio::init();

    QApplication app(argc, argv);

    using qca = QCoreApplication;

    QCoreApplication::setApplicationName("arcane-chat-client");
    QCoreApplication::setApplicationVersion(ARCANE_CHAT_VERSION);

    QCommandLineParser parser;

    parser.setApplicationDescription("The client for Arcane Chat.");
    parser.addHelpOption();
    parser.addVersionOption();

    QCommandLineOption sendFriendRequestOption = {
        QStringList() << "r" << "request",
        qca::translate("main", "Send a friend request to <toxid>."),
        qca::translate("main", "toxid")
    };
    parser.addOption(sendFriendRequestOption);

    parser.process(app);

    // EXAMPLES OF PARSER USE:

    // // fixme
    // parser.addPositionalArgument(
    //     "source", qca::translate("main", "Source file to copy."));
    // const QStringList args = parser.positionalArguments();

    // // A boolean option with a single name (-p)
    // QCommandLineOption showProgressOption = {
    //     "p", qca::translate("main", "Show progress during copy")
    // };
    // parser.addOption(showProgressOption);
    // bool showProgress = parser.isSet(showProgressOption);

    // // A boolean option with multiple names (-f, --force)
    // QCommandLineOption forceOption = {
    //     QStringList() << "f" << "force",
    //     qca::translate("main", "Overwrite existing files.")
    // };
    // parser.addOption(forceOption);
    // bool force = parser.isSet(forceOption);

    // // An option with a value
    // QCommandLineOption targetDirectoryOption = {
    //     QStringList() << "t" << "target-directory",
    //     qca::translate("main", "Copy all source files into <directory>."),
    //     qca::translate("main", "directory")
    // };
    // parser.addOption(targetDirectoryOption);
    // QString targetDir = parser.value(targetDirectoryOption);

    int ret = 1;

    {
        chat::Core core { "/tmp/client_savedata" };

        if(parser.isSet(sendFriendRequestOption)) {
            QByteArray hex;
            hex.append(parser.value(sendFriendRequestOption));
            core.friend_add(QByteArray::fromHex(hex), "placeholder");
        }

        // Tracer* tracer = new Tracer(&core);
        MainWindow* mw = new MainWindow(&core);
        mw->show();

        struct sigaction interrupt;
        memset(&interrupt, 0, sizeof(interrupt));
        interrupt.sa_handler = &handler;
        sigaction(SIGINT, &interrupt, nullptr);
        sigaction(SIGTERM, &interrupt, nullptr);

        ret = app.exec();
    }

    std::cout << "clean shutdown\n";
    return ret;
}
