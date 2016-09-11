#include <iostream>
#include <vector>
#include <cassert>
#include <cstdio>
#include <cstring>
#include <signal.h>

#include <QApplication>
#include <QDebug>

#include "options.hpp"
#include "core.hpp"
#include "utils.hpp"
#include "friend.hpp"

class Server : public QObject {
    Q_OBJECT
public:
    explicit Server(Core* core);
private slots:
    void on_lossless_packet(chat::Friend* friend_number, QByteArray message);
    void on_lossy_packet(chat::Friend* friend_number, QByteArray message);
private:
    Core* core_;
};

#include "server.moc"

Server::Server(chat::Core* core) : core_(core) {
    connect(core, SIGNAL(on_lossless_packet(chat::Friend*, QByteArray)),
            this, SLOT(on_lossless_packet(chat::Friend*, QByteArray)));
    connect(core, SIGNAL(on_lossy_packet(chat::Friend*, QByteArray)),
            this, SLOT(on_lossy_packet(chat::Friend*, QByteArray)));
}

void Server::on_lossless_packet(chat::Friend* fr, QByteArray message) {
    qDebug() << __func__ << fr->friend_number << message.toHex();
    if(message.startsWith("PING")) {
        qDebug() << "PING: " << QString(message);
        message[1] = 'O';
        core_->send_lossless_packet(fr, message);
    }
}

void Server::on_lossy_packet(chat::Friend* fr, QByteArray message) {
    qDebug() << __func__ << fr->friend_number << message.toHex();
    if(message.startsWith("PING")) {
        qDebug() << "PING: " << QString(message);
        message[1] = 'O';
        core_->send_lossy_packet(fr, message);
    }
}

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
        chat::Core core { "/tmp/server_savedata" };
        Tracer* tracer = new Tracer(&core);
        ret = app.exec();
    }

    std::cout << "clean shutdown\n";
    return ret;
}
