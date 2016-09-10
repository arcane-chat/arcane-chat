#ifndef CHAT_FRIEND_H
#define CHAT_FRIEND_H

#include "enums.hpp"

#include <QByteArray>
#include <QString>
#include <QObject>
#include <QTimer>
#include <QList>
#include "stats.hpp"

namespace chat {
    class Core;
    class Friend : public QObject {
        Q_OBJECT
    public:
        Friend(uint32_t friend_number,
               QByteArray pubkey,
               QString name,
               tox::LinkType connection, Core *core);
        void set_connection(tox::LinkType connection);
        void new_message(bool action, QByteArray message);

        uint32_t friend_number;
        QByteArray publickey;
        tox::LinkType connection;
        QString last_message; // temporary until voice works
        void on_pong(qint64 sent, qint64 received, QByteArray payload);
        void on_ping(qint64 sent, QByteArray payload);
        void on_other(qint64 sent);

        void set_username(QString username);
        QString get_username() { return username; }
        Q_PROPERTY(QString username READ get_username WRITE set_username NOTIFY username_changed);

        Stats rtt, offset;

    Q_SIGNALS:
        void connection_changed(tox::LinkType old_state,
                                tox::LinkType new_state);
        void message(bool action, QByteArray message);
        void latency_update();
        void username_changed();
    private Q_SLOTS:
        void too_idle();
    private:
        int random_delay(int min = 10, int range = 30);
        QTimer idle_timer;
        Core *core;
        QString username;
    };

} // namespace chat

#endif // CHAT_FRIEND_H
