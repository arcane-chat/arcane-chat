#pragma once

#include <tox/tox.h>

#include <QObject>
#include <QTimer>
#include <QList>

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
    void handle_lossy_packet(uint32_t friend_number, QByteArray message);
    void handle_lossless_packet(uint32_t friend_number, QByteArray message);
    void handle_friend_connection_status(uint32_t friend_number,
                                         tox::LinkType link);

    const QList<Friend*> get_friends() { return friends; }
    void send_message(uint32_t friend_number, bool action, QString message);
    void save_state();
    void friend_add_norequest(const QByteArray public_key);

    QString username;

signals:
    void on_message(Friend*, bool action, QString message);
    void on_lossless_packet(uint32_t friend_number, QByteArray message);
    void on_lossy_packet(uint32_t friend_number, QByteArray message);
    void new_friend(Friend*);

private slots:
    void check_tox();

private:
    Tox* tox;
    QTimer iterator;
    QList<Friend*> friends;
    std::string savedata_path;
};
} // namespace chat
