#include "core_db.hpp"

#include <QDebug>

using namespace chat;

CoreDb::CoreDb(QString path) {
    int ret;
    char *error = nullptr;
    ret = sqlite3_open(qPrintable(path + "arcane-chat.sqlite"), &db);
    if (ret != SQLITE_OK) {
        throw "unable to open db";
    }

    sqlite3_exec(db, "CREATE TABLE IF NOT EXISTS channels (name)", nullptr, nullptr, &error);
    if (error) {
        qDebug() << error;
        sqlite3_free(error);
        error = nullptr;
    }

    sqlite3_exec(db, "CREATE TABLE IF NOT EXISTS data (name PRIMARY KEY, value)", nullptr, nullptr, &error);
    if (error) {
        qDebug() << error;
        sqlite3_free(error);
        error = nullptr;
    }

    const char *sql = "INSERT OR REPLACE INTO channels (name) VALUES (?)";
    ret = sqlite3_prepare_v2(db, sql, strlen(sql), &update_channel, nullptr);
    if (ret != SQLITE_OK) {
        qDebug() << "prepare error" << ret;
        throw "unable to prepare statement";
    }

    sql = "SELECT value FROM data WHERE name = ?";
    ret = sqlite3_prepare_v2(db, sql, strlen(sql), &read_data, nullptr);
    if (ret != SQLITE_OK) {
        qDebug() << "prepare error" << ret;
        throw "unable to prepare statement";
    }

    sql = "INSERT OR REPLACE INTO data (name,value) VALUES (?,?)";
    ret = sqlite3_prepare_v2(db, sql, strlen(sql), &update_data, nullptr);
    if (ret != SQLITE_OK) {
        qDebug() << "prepare error" << ret;
        throw "unable to prepare statement";
    }

    sql = "SELECT rowid, name FROM channels";
    ret = sqlite3_prepare_v2(db, sql, strlen(sql), &get_all_channels, nullptr);
    if (ret != SQLITE_OK) {
        qDebug() << "prepare error" << ret;
        throw "unable to prepare statement";
    }
}

QList<Channel*> CoreDb::get_channels() {
    int rowid;
    const char *name;
    int name_size;
    bool cont = true;
    QList<Channel*> out;
    Channel *c = nullptr;
    while (cont) {
        int ret = sqlite3_step(get_all_channels);
        switch (ret) {
        case SQLITE_ROW:
            rowid = sqlite3_column_int(get_all_channels, 0);
            name = reinterpret_cast<const char*>(sqlite3_column_text(get_all_channels, 1));
            name_size = sqlite3_column_bytes(get_all_channels, 1);
            qDebug() << rowid << QByteArray(name,name_size);
            c = new Channel;
            c->set_name(QByteArray(name,name_size));
            c->set_rowid(rowid);
            out.append(c);
            break;
        case SQLITE_DONE:
            cont = false;
            break;
        }
    }
    sqlite3_reset(get_all_channels);
    return out;
}

CoreDb::~CoreDb() {
    int ret;
    ret = sqlite3_finalize(update_channel);
    ret = sqlite3_finalize(read_data);
    ret = sqlite3_finalize(update_data);
    ret = sqlite3_finalize(get_all_channels);
    ret = sqlite3_close(db);
    if (ret != SQLITE_OK) {
        qDebug() << "close error";
        throw "unable to open db";
    }
    qDebug() << "db unloaded";
}

void CoreDb::save_channel(Channel *channel) {
    int ret;
    QByteArray name_bytes = channel->name().toUtf8();
    ret = sqlite3_bind_text(update_channel, 1, name_bytes.data(), name_bytes.size(), SQLITE_TRANSIENT);
    ret = sqlite3_step(update_channel);
    if (ret != SQLITE_DONE) {
        qDebug() << "step" << ret;
        throw "unable to insert row";
    }
    ret = sqlite3_reset(update_channel);
    sqlite3_clear_bindings(update_channel);
}

void CoreDb::set_data(QString key, QByteArray value) {
    int ret;
    QByteArray key_bytes = key.toUtf8();
    ret = sqlite3_bind_text(update_data, 1, key_bytes.data(), key_bytes.size(), SQLITE_TRANSIENT);
    ret = sqlite3_bind_blob(update_data, 2, value.data(), value.size(), SQLITE_TRANSIENT);
    ret = sqlite3_step(update_data);
    qDebug() << "ret" << ret;
    if (ret != SQLITE_DONE) {
        qDebug() << "step" << ret;
        throw "unable to insert row";
    }
    ret = sqlite3_reset(update_data);
    sqlite3_clear_bindings(update_data);
    qDebug() << "saved" << value.size() << "bytes" << key;
    //qDebug() << value;
}

QByteArray CoreDb::get_data(QString key) {
    int ret;
    QByteArray key_bytes = key.toUtf8();
    ret = sqlite3_bind_text(read_data, 1, key_bytes.data(), key_bytes.size(), SQLITE_TRANSIENT);
    ret = sqlite3_step(read_data);
    if (ret == SQLITE_ROW) {
        const char *raw_blob = reinterpret_cast<const char*>(sqlite3_column_blob(read_data, 0));
        int size = sqlite3_column_bytes(read_data, 0);
        QByteArray blob(raw_blob, size);
        sqlite3_reset(read_data);
        sqlite3_clear_bindings(read_data);
        return blob;
    } else if (ret == SQLITE_DONE) {
        sqlite3_reset(read_data);
        sqlite3_clear_bindings(read_data);
        return QByteArray();
    } else {
        qDebug() << "unexpected ret" << ret;
        throw "internal error";
    }
}
