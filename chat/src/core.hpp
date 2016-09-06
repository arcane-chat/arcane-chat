#pragma once

namespace tox {
    namespace clib {
#include <tox/tox.h>
    }
}

#include <QObject>
#include <QTimer>
#include <QList>

#include "friend.hpp"

namespace chat {
    class Core : public QObject {
        Q_OBJECT

    public:
        explicit Core(tox::clib::Tox* tox);

        void handle_message(uint32_t friend_number,
                            TOX_MESSAGE_TYPE type,
                            QByteArray message);
        void handle_lossy_packet(uint32_t friend_number, QByteArray message);
        void handle_lossless_packet(uint32_t friend_number, QByteArray message);
        void handle_friend_connection_update(uint32_t friend_number,
                                             TOX_CONNECTION link);

        const QList<Friend*> get_friends() { return friends; }
        void send_message(uint32_t friend_number, bool action, QString message);

        QString username;

    signals:
        void on_message(Friend*, bool action, QString message);
        void on_lossless_packet(uint32_t friend_number, QByteArray message);
        void on_lossy_packet(uint32_t friend_number, QByteArray message);

    private slots:
        void check_tox();

    private:
        Tox* tox;
        QTimer iterator;
        QList<Friend*> friends;
    };
}
