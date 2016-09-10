#include "friend.hpp"
#include "core.hpp"

#include <QDebug>

namespace chat {
    Friend::Friend(uint32_t friend_number,
                   QByteArray pubkey,
                   QString name,
                   tox::LinkType connection, Core *core)
        : friend_number(friend_number), publickey(pubkey), name(name),
          connection(connection), core(core) {
      connect(&idle_timer, SIGNAL(timeout()), this, SLOT(too_idle()));
    }

    void Friend::set_connection(tox::LinkType connection) {
        tox::LinkType old_state = this->connection;
        this->connection = connection;
        if (connection == tox::LinkType::none) {
          idle_timer.stop();
        } else {
          idle_timer.start(random_delay());
        }
        emit connection_changed(old_state, connection);
    }

    void Friend::new_message(bool action, QByteArray message) {
        last_message = message;
        emit this->message(action, message);
    }

    int Friend::random_delay(int min, int range) {
      return (rand() % min + range) * 1000;
    }

    void Friend::too_idle() {
      core->send_ping(this, QByteArray("wake up"));
      idle_timer.start(random_delay());
    }
    void Friend::on_pong(qint64 sent, qint64 received, QByteArray payload) {
      qDebug() << "latency to" << name << "is" << (core->get_uptime() - sent) << "and offset" << (core->get_uptime() - received);
      idle_timer.start(random_delay());
    }
    void Friend::on_ping(qint64 sent, QByteArray payload) {
      qDebug() << "clock offset to" << name << "is" << (sent - core->get_uptime()) << "+/- latency";
      idle_timer.start(random_delay(50,50));
    }
} // namespace chat
