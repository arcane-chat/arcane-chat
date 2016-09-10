#include "stats.hpp"

#include <math.h>
#include <QDebug>

namespace {
    QString shorten(qint64 ns) {
        if ((ns > -1000) & (ns < 1000)) return QString("%1ns").arg(ns);
        if ((ns > -1000000) & (ns < 1000000)) return QString("%1us").arg(ns/1000);
        if ((ns > -1000000000) & (ns < 1000000000)) return QString("%1ms").arg(ns/1000000);
        qint64 sec = ns/1000000000;
        if ((sec > -60) & (sec < 60)) return QString("%1sec").arg(sec);
        return QString("%1min").arg(sec/60);
    }
}

QString Stats::toString() {
	return QString("%1 ± %2").arg(shorten(average())).arg(shorten(stddev()));
}

qint64 Stats::average() {
    if (list.size() == 0) return 0;
    qint64 total = 0;
    for (qint64 x : list) total += x;
    return total / list.size();
}

void Stats::append(qint64 i) {
	list.append(i);
	if (list.size() > 10) list.removeFirst();
	qDebug() << list;
}

qint64 Stats::stddev() {
	qint64 avg = average();
	qint64 sum = 0;
	for (qint64 x : list) {
		sum += (avg - x) * (avg - x);
	}
	return sqrt(sum);
}