#include "kisscache.hpp"

#include <sqlite3.h>

#include <QDebug>

using namespace streaming;

class streaming::KissCachePrivate {
public:
    sqlite3 *db;
    sqlite3_stmt *insert1;
};

KissCache::KissCache() {
	int ret;
	char *error = nullptr;
	d = new KissCachePrivate();
	ret = sqlite3_open("kisscache.sqlite", &d->db);
	if (ret != SQLITE_OK) {
		throw "unable to open db";
	}
	sqlite3_exec(d->db, "CREATE TABLE IF NOT EXISTS episodes (id INTEGER PRIMARY KEY, url, title, series)", nullptr, nullptr, &error);
	if (error) {
		qDebug() << error;
		sqlite3_free(error);
		error = nullptr;
	}
	const char *sql = "INSERT OR REPLACE INTO episodes (id, url, title, series) VALUES (?,?,?,?)";
	ret = sqlite3_prepare_v2(d->db, sql, strlen(sql), &d->insert1, nullptr);
	if (ret != SQLITE_OK) {
        qDebug() << "prepare error" << ret;
		throw "unable to prepare statement";
	}
}

KissCache::~KissCache() {
	int ret;
    ret = sqlite3_finalize(d->insert1);
	ret = sqlite3_close(d->db);
	if (ret != SQLITE_OK) {
		throw "unable to open db";
	}
	delete d;
}

void KissCache::add_episode(int id, QString url, QString title, QString series) {
    int ret;
    QByteArray url_bytes = url.toUtf8();
    QByteArray title_bytes = title.toUtf8();
    QByteArray series_bytes = series.toUtf8();

    ret = sqlite3_bind_int(d->insert1, 1, id);
    ret = sqlite3_bind_text(d->insert1, 2, url_bytes.data(), url_bytes.size(), SQLITE_TRANSIENT);
    ret = sqlite3_bind_text(d->insert1, 3, title_bytes.data(), title_bytes.size(), SQLITE_TRANSIENT);
    ret = sqlite3_bind_text(d->insert1, 4, series_bytes.data(), series_bytes.size(), SQLITE_TRANSIENT);
    ret = sqlite3_step(d->insert1);
    if (ret != SQLITE_DONE) {
        qDebug() << "step" << ret;
        throw "unable to insert row";
    }
    ret = sqlite3_reset(d->insert1);
}
