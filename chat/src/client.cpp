#include <iostream>
#include <boost/algorithm/hex.hpp>
#include <vector>
#include <tox/tox.h>
#include <cassert>
#include <cstdio>
#include <cstring>
#include <signal.h>

#include <QApplication>

#include "options.hpp"
#include "core.hpp"
#include "utils.hpp"
#include "friend.hpp"
#include "mainwindow.h"

void handler(int signum) {
    std::cout << "Quitting...\n";
    QCoreApplication::quit();
}

int main(int argc, char** argv) {
    QApplication app(argc, argv);
    struct sigaction interrupt;
    memset(&interrupt, 0, sizeof(interrupt));
    interrupt.sa_handler = &handler;
    sigaction(SIGINT, &interrupt, nullptr);
    sigaction(SIGTERM, &interrupt, nullptr);

    int ret = 1;

    {
        chat::Core core { "/tmp/client_savedata" };
        // Tracer* tracer = new Tracer(&core);
        QList<chat::Friend*> friends = core.get_friends();
        MainWindow* mw = new MainWindow(friends, &core);
        mw->show();
        ret = app.exec();
    }

    std::cout << "clean shutdown\n";
    return ret;
}
