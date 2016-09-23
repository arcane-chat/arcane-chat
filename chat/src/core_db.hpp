#pragma once

#include "channel.hpp"
#include "db.hpp"

#include <sqlite3.h>

namespace chat {

//! object to manage the database state for a Core instance
class CoreDb {
public:
    explicit CoreDb(QString path);
    ~CoreDb();

    //! read all channels
    QList<Channel*> get_channels();
    //! write a single channel to the channels table
    void save_channel(Channel *channel);

    //! read a key=value pair from the data table
    QByteArray get_data(QString key);
    //! write a key=value pair to the data table
    void set_data(QString key, QByteArray value);
private:
    db::PreparedQuery *set_data_, *get_data_, *update_channel, *get_all_channels;
    db::Db *db_;
};
}
