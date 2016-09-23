#pragma once

#include <QString>

#include <sqlite3.h>

#include <exception>

namespace db {

class PreparedQuery;

class Db {
public:
    explicit Db(QString path);
    void exec(QString sql);
    void close();
private:
    sqlite3 *db_;
    friend class PreparedQuery;
};

class SimpleException : public std::exception {
public:
    SimpleException(const char *msg);
    virtual const char *what() const throw();
private:
    const char *what_;
};

class PrepareException : public SimpleException {
public:
    PrepareException(const char *msg) : SimpleException(msg) {}
};
class DbException : public SimpleException {};

class PreparedQuery {
public:
    PreparedQuery(db::Db *db, QString sql);
    ~PreparedQuery();
    void finalize();
    void bind(int column, QString value);
    void bind(int column, QByteArray value);
    int step();
    void reset();
    void clear_bindings();
    QByteArray column_blob(int column);
    QString column_text(int column);
private:
    sqlite3_stmt *stmt_;
};

}
