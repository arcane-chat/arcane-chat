#pragma once

#include <QString>

#include <sqlite3.h>

#include <exception>

namespace db {

class PreparedQuery;

//! a thin wrapper around a sqlite3 handle
class Db {
public:
    /**
     * opens a database file r/w
     * @param[in] path the path to the database file
     */
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

class DbException : public SimpleException {
public:
    DbException(const char *msg) : SimpleException(msg) {}
};

//! a thin wrapper around sqlite3_stmt
class PreparedQuery {
public:
    PreparedQuery(db::Db *db, QString sql);
    ~PreparedQuery();
    void finalize();
    //! binds a text value
    void bind(int column, QString value);
    //! binds a blob value
    void bind(int column, QByteArray value);
    //! executes a single step of the query
    int step();
    //! resets a query so it may be executed again
    void reset();
    //! clears all bindings
    void clear_bindings();
    //! converts to blob and returns the value
    QByteArray column_blob(int column);
    //! converts to text and returns the value
    QString column_text(int column);
private:
    sqlite3_stmt *stmt_;
};

}
