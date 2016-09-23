#include "core_db.hpp"

#include <QDebug>
#include <sodium.h>

using namespace chat;
using namespace db;

CoreDb::CoreDb(QString path) {
    db_ = new Db(path + "arcane-chat.sqlite");

    db_->exec(QStringLiteral("CREATE TABLE IF NOT EXISTS channels (name, pubkey PRIMARY KEY, privkey)"));
    db_->exec(QStringLiteral("CREATE TABLE IF NOT EXISTS data (name PRIMARY KEY, value)"));

    set_data_ = new PreparedQuery(db_, QStringLiteral("INSERT OR REPLACE INTO data (name,value) VALUES (?,?)"));
    get_data_ = new PreparedQuery(db_, QStringLiteral("SELECT value FROM data WHERE name = ?"));

    update_channel = new PreparedQuery(db_, QStringLiteral("INSERT OR REPLACE INTO channels (name, pubkey, privkey) VALUES (?,?,?)"));
    get_all_channels = new PreparedQuery(db_, QStringLiteral("SELECT name, pubkey, privkey name FROM channels"));
}

CoreDb::~CoreDb() {
    delete update_channel;
    delete set_data_;
    delete get_data_;
    delete get_all_channels;
    db_->close();
    qDebug() << "db unloaded";
}

QList<Channel*> CoreDb::get_channels() {
    QString name;
    QByteArray pubkey, privkey;
    bool cont = true;
    QList<Channel*> out;
    Channel *c = nullptr;
    while (cont) {
        int ret = get_all_channels->step();
        switch (ret) {
        case SQLITE_ROW:
            name = get_all_channels->column_text(0);
            pubkey = get_all_channels->column_blob(1);
            privkey = get_all_channels->column_blob(2);

            qDebug() << name;
            c = new Channel;
            c->set_name(name);

            if (pubkey.size() == crypto_sign_PUBLICKEYBYTES) {
                c->set_pubkey(pubkey);
            }
            if (privkey.size() == crypto_sign_SECRETKEYBYTES) {
                c->set_privkey(privkey);
            }
            out.append(c);
            break;
        case SQLITE_DONE:
            cont = false;
            break;
        }
    }
    get_all_channels->reset();
    return out;
}

void CoreDb::save_channel(Channel *channel) {
    int ret;
    QByteArray name_bytes = channel->name().toUtf8();
    update_channel->bind(1, channel->name());
    update_channel->bind(2, channel->pubkey());
    update_channel->bind(3, channel->privkey());
    ret = update_channel->step();
    if (ret != SQLITE_DONE) {
        qDebug() << "step" << ret;
        throw "unable to insert row";
    }
    update_channel->reset();
    update_channel->clear_bindings();
}

void CoreDb::set_data(QString key, QByteArray value) {
    int ret;
    QByteArray key_bytes = key.toUtf8();
    set_data_->bind(1, key);
    set_data_->bind(2, value);
    ret = set_data_->step();
    if (ret != SQLITE_DONE) {
        qDebug() << "step" << ret;
        throw "unable to insert row";
    }
    set_data_->reset();
    set_data_->clear_bindings();
    qDebug() << "saved" << value.size() << "bytes" << key;
    //qDebug() << value;
}

QByteArray CoreDb::get_data(QString key) {
    int ret;
    QByteArray key_bytes = key.toUtf8();
    get_data_->bind(1, key);
    ret = get_data_->step();
    if (ret == SQLITE_ROW) {
        QByteArray blob = get_data_->column_blob(0);
        get_data_->reset();
        get_data_->clear_bindings();
        return blob;
    } else if (ret == SQLITE_DONE) {
        get_data_->reset();
        get_data_->clear_bindings();
        return QByteArray();
    } else {
        qDebug() << "unexpected ret" << ret;
        throw "internal error";
    }
}
