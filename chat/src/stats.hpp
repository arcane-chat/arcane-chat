#include <QList>

class Stats {
public:
	QString toString();
	qint64 average();
	qint64 stddev();
	void append(qint64);
	void clear();
	void shift(qint64 offset);
private:
	QList<qint64> list;
};
