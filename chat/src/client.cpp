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
#include "mainwindow.hpp"

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
        if(argc == 2) {
            QByteArray hex;
            hex.append(QCoreApplication::arguments().at(1));
            core.friend_add(QByteArray::fromHex(hex), "quq");
            for(int i = 0; i < 100; i++) {
                for(chat::Friend* fr : core.get_friends()) {
                    if(fr->connection != tox::LinkType::none) {
                        core.send_lossless_packet(fr, "PING123456_LOSSLESS");
                        core.send_lossy_packet(fr, "PING654321_LOSSY");
                    }
                }
                usleep(5000);
            }
        }
        Tracer* tracer = new Tracer(&core);
        MainWindow* mw = new MainWindow(&core);
        mw->show();
        ret = app.exec();
    }

    std::cout << "clean shutdown\n";
    return ret;
}
