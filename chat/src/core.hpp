#pragma once

#include <tox/tox.h>

#include <QObject>
#include <QTimer>
#include <QList>

#include "friend.h"

namespace chat {
    class Core : public QObject {
        Q_OBJECT
    public:
        Core(Tox* tox);
        void handleMessage(uint32_t friend_number,
                           TOX_MESSAGE_TYPE type,
                           QByteArray message);
        void handleLossyPacket(uint32_t friend_number, QByteArray message);
        void handleLosslessPacket(uint32_t friend_number, QByteArray message);
        void handle_friend_connection_update(uint32_t friend_number,
                                             TOX_CONNECTION link);
        const QList<Friend*> getFriends() { return friends; }
        void send_message(uint32_t friend_number, bool action, QString message);

        QString username;
    signals:
        void on_message(Friend*, bool action, QString message);
        void onLosslessPacket(uint32_t friend_number, QByteArray message);
        void onLossyPacket(uint32_t friend_number, QByteArray message);
    private slots:
        void checkTox();

    private:
        Tox* tox;
        QTimer iterator;
        QList<Friend*> friends;
    };
}
