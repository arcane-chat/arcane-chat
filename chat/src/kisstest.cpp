#include "kiss.hpp"

#include <QCoreApplication>
#include <QFile>

int main(int argc, char **argv) {
	QCoreApplication app(argc,argv);
	streaming::Kiss site("KissAnime",QUrl("http://kissanime.to"));
#if 0
	site.parseSeries("RWBY-Chibi");
	return app.exec();
#endif

#if 1
	QFile fh("sample.html");
	fh.open(QFile::ReadOnly);
	site.parse_series("RWBY-Chibi",fh.readAll());
	return 0;
#endif
}