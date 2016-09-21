#pragma once

#include <QObject>
#include <QString>
#include <QUrl>
#include <QNetworkReply>
#include <QTimer>

class QNetworkAccessManager;

namespace streaming {

class KissParseRequest;

class Kiss : public QObject {
Q_OBJECT
friend class KissParseRequest;
public:
	explicit Kiss(QString name, QUrl root);
	void parseSeries(QString seriesName);
public slots:
	//void replyFinished(QNetworkReply *reply);
	void parse_series(QString seriesName, QByteArray data);
private:
	QUrl root_;
	QString name_;
	QNetworkAccessManager *manager;
};


class KissParseRequest : public QObject {
Q_OBJECT
public:
	explicit KissParseRequest(Kiss *parent, QString seriesName);
signals:
	void got_page(QString seriesName, QByteArray data);
private slots:
	void finished();
	void send_answer();
private:
	QNetworkReply *reply_;
	Kiss *kiss_;
	QString seriesName_;
	QUrl absolute;
	QTimer delay;
	QUrl final_answer;
	bool done;
};

}