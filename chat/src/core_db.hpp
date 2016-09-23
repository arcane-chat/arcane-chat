#pragma once

#include "channel.hpp"
#include "db.hpp"

#include <sqlite3.h>

namespace chat {
class CoreDb {
public:
    explicit CoreDb(QString path);
    ~CoreDb();
    void save_channel(Channel *channel);
    QByteArray get_data(QString key);
    void set_data(QString key, QByteArray value);
    QList<Channel*> get_channels();
private:
    db::PreparedQuery *set_data_, *get_data_, *update_channel, *get_all_channels;
    db::Db *db_;
};
}
