#include <QList>
#include <QString>

namespace util {

//! class to manage averages and stddev's
class Stats {
public:
    //! returns a string with average and stddev
	QString toString();
	qint64 average();
	qint64 stddev();
	void append(qint64);
	void clear();
	//! apply an offset to all records, to change average but not stddev
	void shift(qint64 offset);
	int count();
	//! render a nanosecond number as a human readable string
	static QString shorten(qint64 n);
private:
	QList<qint64> list;
};

}
