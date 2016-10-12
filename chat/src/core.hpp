#pragma once

#include "callcontrol.hpp"
#include "channel.hpp"
#include "enums.hpp"
#include "friend.hpp"
#include "network.pb.h"

#include <tox/tox.h>

#include <fstream>

#include <QObject>
#include <QTimer>
#include <QMap>
#include <QElapsedTimer>

namespace chat {
class CoreDb;

//! the core of arcane-chat, for both servers and clients
class Core : public QObject {
    Q_OBJECT

public:
    explicit Core(QString path);
    ~Core();

    void handle_message(uint32_t friend_number,
                        tox::MessageType type,
                        QByteArray message);
    void handle_lossy_packet(Friend* fr, QByteArray message);
    void handle_lossless_packet(Friend* fr, QByteArray message);
    void handle_friend_connection_status(Friend* fr, tox::LinkType link);
    void send_message(uint32_t friend_number, bool action, QString message);
    void save_state();
    void friend_add_norequest(QByteArray public_key);
    void friend_add(QByteArray tox_id, std::string message);
    void send_lossy_packet(Friend*, QByteArray data);
    void send_lossless_packet(Friend*, QByteArray data);
    void call_start(Friend *fr);
    void call_data(Friend *fr, QByteArray data);
    void call_stop(Friend *fr);
    void send_packet(Friend *fr, arcane::network::Methods methodid,
                     ::google::protobuf::Message *payload = 0);
    qint64 get_uptime();
    void set_username(QString username);
    void open_call_control(Friend *fr);
    void add_owned_channel(chat::Channel *channel);
    void join_channel(chat::Channel *channel);

    const QMap<uint32_t, Friend*>& friends() { return friends_; }
    const QList<Channel*>& channels() { return channels_; };

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
    QMap<uint32_t, Friend*> friends_;
    QElapsedTimer uptime;
    quint64 uptime_offset;
    QMap<Friend*,CallControl*> calls_;
    QList<Channel*> channels_;
    CoreDb *db;
    Channel *current_channel_;
};
} // namespace chat
