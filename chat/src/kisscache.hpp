#pragma once

#include <QObject>

namespace streaming {
class KissCachePrivate;

class KissCache : public QObject {
Q_OBJECT
public:
	explicit KissCache();
	~KissCache();
	void add_episode(int id, QString url, QString title, QString series);

private:
	KissCachePrivate *d;
};
}