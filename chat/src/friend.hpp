#ifndef CHAT_FRIEND_H
#define CHAT_FRIEND_H

#include <QByteArray>
#include <QString>
#include <QObject>

namespace chat {

    enum class Link { none, tcp, udp };

    class Friend : public QObject {
        Q_OBJECT
    public:
        Friend(uint32_t friend_number,
               QByteArray pubkey,
               QString name,
               Link connection);
        void set_connection(Link connection);
        void new_message(bool action, QByteArray message);

        uint32_t friend_number;
        QByteArray publickey;
        QString name;
        Link connection;
        QString last_message; // temporary until voice works
    signals:
        void connection_changed(Link old_state, Link new_state);
        void message(bool action, QByteArray message);
    };

} // namespace chat

#endif // CHAT_FRIEND_H
