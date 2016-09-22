#include "kiss.hpp"

#include <QCoreApplication>
#include <QFile>

class tester : public QObject {
Q_OBJECT
public:
	explicit tester(streaming::Kiss *site) : site(site) {
		connect(site, SIGNAL(series_parsed(QString)), this, SLOT(show_done(QString)));
	};
	QStringList queue;
	streaming::Kiss *site;
public slots:
	void show_done(QString name) {
		if (queue.size() > 0) {
			QString next = queue.takeFirst();
			site->parseSeries(next);
		} else {
			QCoreApplication::quit();
		}
	}
};

#include "kisstest.moc"

int main(int argc, char **argv) {
	QCoreApplication app(argc,argv);
	streaming::Kiss site("KissAnime",QUrl("http://kissanime.to"));
#if 1
	tester t(&site);
	t.queue << "RWBY-Chibi" << "New-Game" << "Re-Zero-kara-Hajimeru-Isekai-Seikatsu";
	//site.parseSeries("RWBY-Chibi");
	t.show_done("");
	return app.exec();
#endif

#if 0
	QFile fh("sample.html");
	fh.open(QFile::ReadOnly);
	site.parse_series("RWBY-Chibi",fh.readAll());
	return 0;
#endif
}