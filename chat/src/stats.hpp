#include <QList>

class Stats {
public:
	QString toString();
	qint64 average();
	qint64 stddev();
	void append(qint64);
private:
	QList<qint64> list;
};