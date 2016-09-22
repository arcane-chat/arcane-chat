#pragma once

#include "channel.hpp"

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
    sqlite3 *db;
    sqlite3_stmt *update_channel, *read_data, *update_data, *get_all_channels;
};
}
