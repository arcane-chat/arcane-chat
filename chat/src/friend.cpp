#include "friend.hpp"
#include "core.hpp"

#include <QDebug>

namespace chat {
    Friend::Friend(uint32_t friend_number,
                   QByteArray pubkey,
                   QString name,
                   tox::LinkType connection, Core *core)
        : friend_number(friend_number), publickey(pubkey),
          connection(connection), core(core), username(name) {
      connect(&idle_timer, SIGNAL(timeout()), this, SLOT(too_idle()));
    }

    void Friend::set_connection(tox::LinkType connection) {
        tox::LinkType old_state = this->connection;
        this->connection = connection;
        if (connection == tox::LinkType::none) {
          idle_timer.stop();
          offset.clear();
        } else {
          idle_timer.start(random_delay(0,10));
        }
        emit connection_changed(old_state, connection);
    }

    void Friend::new_message(bool action, QByteArray message) {
        last_message = message;
        emit this->message(action, message);
    }

    int Friend::random_delay(int min, int range) {
      int ret = (rand() % range + min) * 1000;
      return ret;
    }

    void Friend::too_idle() {
      core->send_ping(this, QByteArray("wake up"));
      idle_timer.start(random_delay(10,10));
    }

    void Friend::on_pong(qint64 sent, qint64 received, QByteArray payload) {
      qint64 now = core->get_uptime();

      qint64 rtt = now - sent;
      //double in_ms = (double)rtt / 1000000;
      qint64 offset = now - received;
      //double offset_sec = (double)offset / 1000000000;
      //qDebug() << "latency to" << name << "is" << in_ms << "ms and offset" << offset_sec << "seconds";
      idle_timer.start(random_delay());

      this->rtt.append(rtt);
      this->offset.append(offset);

      emit latency_update();
    }

    // positive offsets, the sender is lagging behind
    void Friend::on_ping(qint64 sent, QByteArray payload) {
      qint64 now = core->get_uptime();

      qint64 offset = now - sent;
      //double offset_sec = (double)offset / 1000000000;
      //qDebug() << "clock offset to" << name << "is" << offset_sec << "seconds +/- latency";
      idle_timer.start(random_delay(10,20));

      this->offset.append(offset);

      emit latency_update();
    }

    void Friend::on_other(qint64 sent) {
        qint64 offset = core->get_uptime() - sent;
        this->offset.append(offset);
        emit latency_update();
    }

    void Friend::set_username(QString username) {
      this->username = username;
      emit username_changed();
    }
} // namespace chat
