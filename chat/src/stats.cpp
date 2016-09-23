#include "stats.hpp"

#include <math.h>
#include <QDebug>

QString Stats::shorten(qint64 ns) {
    if ((ns > -1000) & (ns < 1000)) return QStringLiteral("%1ns").arg(ns);
    if ((ns > -1000000) & (ns < 1000000)) return QStringLiteral("%1μs").arg(ns/1000);
    if ((ns > -1000000000) & (ns < 1000000000)) return QStringLiteral("%1ms").arg(ns/1000000);
    qint64 sec = ns/1000000000;
    if ((sec > -60) & (sec < 60)) return QStringLiteral("%1sec").arg(sec);
    return QStringLiteral("%1min").arg(sec/60);
}

QString Stats::toString() {
	return QStringLiteral("%1 ± %2").arg(shorten(average())).arg(shorten(stddev()));
}

qint64 Stats::average() {
    if (list.size() == 0) return 0;
    qint64 total = 0;
    for (qint64 x : list) total += x;
    return total / list.size();
}

void Stats::append(qint64 i) {
	list.append(i);
	if (list.size() > 200) list.removeFirst();
	//qDebug() << list;
}

qint64 Stats::stddev() {
	double avg = average();
	double sum = 0;
	for (qint64 x : list) {
		sum += (avg - x) * (avg - x);
	}
	return sqrt(sum);
}

void Stats::clear() {
    list.clear();
}

void Stats::shift(qint64 offset) {
    for (qint64 &x : list) {
        x -= offset;
    }
}

int Stats::count() {
    return list.size();
}