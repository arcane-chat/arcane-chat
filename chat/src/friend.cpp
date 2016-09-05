#include "friend.h"

namespace chat {

    Friend::Friend(uint32_t friend_number,
                   QByteArray pubkey,
                   QString name,
                   Link connection)
        : friend_number(friend_number), publickey(pubkey), name(name),
          connection(connection) {}

    void Friend::set_connection(Link connection) {
        Link old_state = this->connection;
        this->connection = connection;
        emit connection_changed(old_state, connection);
    }

    void Friend::new_message(bool action, QByteArray message) {
        last_message = message;
        emit this->message(action, message);
    }
} // namespace chat
