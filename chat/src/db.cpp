#include "db.hpp"

#include <QDebug>

using namespace db;

Db::Db(QString path) {
    int ret;
    ret = sqlite3_open(qPrintable(path), &db_);
    if (ret != SQLITE_OK) {
        throw "unable to open db";
    }
}

void Db::exec(QString sql) {
    char *error = nullptr;
    sqlite3_exec(db_, qPrintable(sql), nullptr, nullptr, &error);
    if (error) {
        qDebug() << error;
        sqlite3_free(error);
        error = nullptr;
    }
}

void Db::close() {
    if (db_) {
        int ret = sqlite3_close(db_);
        if (ret != SQLITE_OK) {
            qDebug() << "close error";
            throw "unable to open db";
        }
    }
    db_ = nullptr;
}

SimpleException::SimpleException(const char *what) : what_(what) {
}

const char *SimpleException::what() const throw() {
    return what_;
}

PreparedQuery::PreparedQuery(db::Db *db, QString sql) {
    QByteArray sql_utf8 = sql.toUtf8();
    int ret = sqlite3_prepare_v2(db->db_, sql_utf8.constData(), sql_utf8.size(), &stmt_, nullptr);
    if (ret != SQLITE_OK) {
        qDebug() << "prepare error" << ret;
        throw new PrepareException("unable to prepare statement");
    }
}

PreparedQuery::~PreparedQuery() {
    if (stmt_) finalize();
}

void PreparedQuery::finalize() {
    if (stmt_) sqlite3_finalize(stmt_);
    stmt_ = nullptr;
}

void PreparedQuery::bind(int column, QByteArray value) {
    int ret = sqlite3_bind_blob(stmt_, column, value.constData(), value.size(), SQLITE_TRANSIENT);
    if (ret != SQLITE_OK) {
        qDebug() << "bind error" << ret;
        throw new PrepareException("unable to bind a value");
    }
}

void PreparedQuery::bind(int column, QString str) {
    QByteArray value = str.toUtf8();
    int ret = sqlite3_bind_text(stmt_, column, value.constData(), value.size(), SQLITE_TRANSIENT);
    if (ret != SQLITE_OK) {
        qDebug() << "bind error" << ret;
        throw new PrepareException("unable to bind a value");
    }
}

int PreparedQuery::step() {
    return sqlite3_step(stmt_);
}

void PreparedQuery::reset() {
    int ret = sqlite3_reset(stmt_);
    if (ret != SQLITE_OK) {
        qDebug() << "reset error" << ret;
        throw new PrepareException("unable to reset statement");
    }
}

void PreparedQuery::clear_bindings() {
    int ret = sqlite3_clear_bindings(stmt_);
    if (ret != SQLITE_OK) {
        qDebug() << "clear error" << ret;
        throw new PrepareException("unable to reset bindings");
    }
}

QByteArray PreparedQuery::column_blob(int column) {
    const char *raw_blob = reinterpret_cast<const char*>(sqlite3_column_blob(stmt_, column));
    int size = sqlite3_column_bytes(stmt_, column);
    return QByteArray(raw_blob, size);
}

QString PreparedQuery::column_text(int column) {
    const unsigned char *raw = sqlite3_column_text(stmt_, column);
    int size = sqlite3_column_bytes(stmt_, column);
    return QString::fromUtf8(reinterpret_cast<const char*>(raw), size);
}
