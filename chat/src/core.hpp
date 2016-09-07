#pragma once

#include <tox/tox.h>

#include <QObject>
#include <QTimer>
#include <QMap>

#include <boost/filesystem.hpp>

#include "enums.hpp"
#include "friend.hpp"

struct Tox;

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
    const QMap<uint32_t, Friend*> get_friends() { return friends; }
    void send_message(uint32_t friend_number, bool action, QString message);
    void save_state();
    void friend_add_norequest(const QByteArray public_key);
    void send_lossy_packet(Friend*, QByteArray data);
    void send_lossless_packet(Friend*, QByteArray data);
    void call_start(Friend *fr);
    void call_data(Friend *fr, QByteArray data);
    void call_stop(Friend *fr);

    QString username;

Q_SIGNALS:
    void on_message(Friend* fr, bool action, QString message);
    void on_lossless_packet(Friend* fr, QByteArray message);
    void on_lossy_packet(Friend* fr, QByteArray message);
    void on_new_friend(Friend* fr);

private Q_SLOTS:
    void check_tox();

private:
    Tox* tox;
    QTimer iterator;
    QMap<uint32_t, Friend*> friends;
    std::string savedata_path;
};
} // namespace chat
