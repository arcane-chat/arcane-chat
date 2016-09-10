#pragma once

#include "network.pb.h"
#include "enums.hpp"
#include "friend.hpp"

#include <tox/tox.h>

#include <boost/filesystem.hpp>

#include <QObject>
#include <QTimer>
#include <QMap>
#include <QElapsedTimer>

namespace chat {
class Core : public QObject {
    Q_OBJECT

public:
    explicit Core(std::string path);
    ~Core();

    void handle_message(uint32_t friend_number,
                        tox::MessageType type,
                        QByteArray message);
    void handle_lossy_packet(Friend* fr, QByteArray message);
    void handle_lossless_packet(Friend* fr, QByteArray message);
    void handle_friend_connection_status(Friend* fr, tox::LinkType link);
    const QMap<uint32_t, Friend*>& get_friends() { return friends; }
    void send_message(uint32_t friend_number, bool action, QString message);
    void save_state();
    void friend_add_norequest(QByteArray public_key);
    void friend_add(QByteArray tox_id, std::string message);
    void send_lossy_packet(Friend*, QByteArray data);
    void send_lossless_packet(Friend*, QByteArray data);
    void call_start(Friend *fr);
    void call_data(Friend *fr, QByteArray data);
    void call_stop(Friend *fr);
    void call_control(uint8_t type, Friend *fr, QByteArray data);
    void send_packet(Friend *fr, Arcane::Methods methodid, ::google::protobuf::Message *payload=0);
    qint64 get_uptime();
    void set_username(QString username);

    QString username;

Q_SIGNALS:
    void on_message(Friend* fr, bool action, QString message);
    void on_lossless_packet(Friend* fr, QByteArray message);
    void on_lossy_packet(Friend* fr, QByteArray message);
    void on_new_friend(Friend* fr);
    void on_pong(Friend *fr, qint64 sent, qint64 received, QByteArray payload);

public Q_SLOTS:
    void send_ping(Friend *fr, QByteArray payload);
private Q_SLOTS:
    void check_tox();
    void feed_tox(int sock);
    void sync_clock();

private:
    void shift_clock(qint64 offset);

    Tox* tox;
    QTimer iterator;
    QMap<uint32_t, Friend*> friends;
    std::string savedata_path;
    QElapsedTimer uptime;
    quint64 uptime_offset;
};
} // namespace chat
