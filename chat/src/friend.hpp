#ifndef CHAT_FRIEND_H
#define CHAT_FRIEND_H

#include <QByteArray>
#include <QString>
#include <QObject>
#include "enums.hpp"

namespace chat {
    class Friend : public QObject {
        Q_OBJECT
    public:
        Friend(uint32_t friend_number,
               QByteArray pubkey,
               QString name,
               tox::LinkType connection);
        void set_connection(tox::LinkType connection);
        void new_message(bool action, QByteArray message);

        uint32_t friend_number;
        QByteArray publickey;
        QString name;
        tox::LinkType connection;
        QString last_message; // temporary until voice works
    signals:
        void connection_changed(tox::LinkType old_state,
                                tox::LinkType new_state);
        void message(bool action, QByteArray message);
    };

} // namespace chat

#endif // CHAT_FRIEND_H
