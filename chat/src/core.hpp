#pragma once

#include <tox/tox.h>

#include <QObject>
#include <QTimer>

namespace chat {
    class Core : public QObject {
    Q_OBJECT
    public:
        Core(Tox *tox);
        void handleMessage(uint32_t friend_number, TOX_MESSAGE_TYPE type,
                        QByteArray message);
        void handleLossyPacket(uint32_t friend_number, QByteArray message);
        void handleLosslessPacket(uint32_t friend_number, QByteArray message);
    signals:
        void onMessage(uint32_t friend_number, TOX_MESSAGE_TYPE type, QString message);
        void onLosslessPacket(uint32_t friend_number, QByteArray message);
        void onLossyPacket(uint32_t friend_number, QByteArray message);
    private slots:
        void checkTox();
    private:
        Tox* tox;
        QTimer iterator;
    };
}
