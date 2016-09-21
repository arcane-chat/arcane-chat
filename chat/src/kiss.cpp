#include "kiss.hpp"

#include <QNetworkAccessManager>
#include <QNetworkRequest>
#include <QDebug>
#include <QScriptEngine>
#include <QFile>
#include <QCoreApplication>

using namespace streaming;

//static const char * user_agent = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.116 Safari/537.36";

static QString defeatJSTest(QString input_js) {
    QScriptEngine engine;
    QFile fh("wrapper.js");
    fh.open(QFile::ReadOnly);
    QString wrapper = QString(fh.readAll());

    engine.evaluate(wrapper, "wrapper.js");
    if (engine.hasUncaughtException()) {
        qDebug() << engine.uncaughtException().toString();
    }

    engine.evaluate(input_js, "kiss");
    if (engine.hasUncaughtException()) {
        qDebug() << engine.uncaughtException().toString();
    }

    QScriptValue out = engine.evaluate("answerobj.value");
    if (engine.hasUncaughtException()) {
        qDebug() << engine.uncaughtException().toString();
    }
    return out.toString();
}

Kiss::Kiss(QString name, QUrl root) : root_(root), name_(name) {
    manager = new QNetworkAccessManager(this);
    //connect(manager, SIGNAL(finished(QNetworkReply*)), this, SLOT(replyFinished(QNetworkReply*)));
}

void Kiss::parseSeries(QString seriesName) {
    KissParseRequest *parser = new KissParseRequest(this, seriesName);
    connect(parser, SIGNAL(got_page(QString,QByteArray)), this, SLOT(parse_series(QString,QByteArray)));
}

void Kiss::parse_series(QString seriesName, QByteArray data) {
    // QFile fh("out.html");
    // fh.open(QFile::WriteOnly);
    // fh.write(data);
    QString str = data;
    QRegExp expr("<a  href=\"([^\"]*)\"\r\n *title=\"[^\"]*\">\r\n {40}([^<]*)</a>");
    int index = 0;
    while (true) {
        int offset = expr.indexIn(str.mid(index));
        if (offset == -1) break;
        index += offset;
        qDebug() << expr.capturedTexts().mid(1,2);
        index += expr.matchedLength();
    }
    QCoreApplication::quit();
}

KissParseRequest::KissParseRequest(Kiss *kiss, QString seriesName) : reply_(0), kiss_(kiss), seriesName_(seriesName), done(false) {
    delay.setSingleShot(true);
    connect(&delay, SIGNAL(timeout()), this, SLOT(send_answer()));

    QUrl relative(QString("/Anime/") + seriesName);
    absolute = QUrl(kiss_->root_.resolved(relative));
    qDebug() << absolute;
    QNetworkRequest req(absolute);
    //req.setHeader(QNetworkRequest::UserAgentHeader, user_agent);
    reply_ = kiss_->manager->get(req);
    connect(reply_, SIGNAL(finished()), this, SLOT(finished()));
}

void KissParseRequest::finished() {
    qDebug() << reply_->url() << reply_->error();
    qDebug() << reply_->rawHeaderList();
    switch (reply_->error()) {
    case QNetworkReply::NoError: {
        int status = reply_->attribute(QNetworkRequest::HttpStatusCodeAttribute).toInt();
        qDebug() << status;
        switch (status) {
        case 200:
            emit got_page(seriesName_, reply_->readAll());
            reply_->deleteLater();
            this->deleteLater();
            break;
        case 302:
            qDebug() << reply_->readAll();
            qDebug() << reply_->header(QNetworkRequest::LocationHeader);
            qDebug() << reply_->rawHeader("Location");
            reply_->deleteLater();
            reply_ = kiss_->manager->get(QNetworkRequest(absolute));
            connect(reply_, SIGNAL(finished()), this, SLOT(finished()));
            break;
        }
        break; }
    case QNetworkReply::ServiceUnavailableError:
        if (done) {
            qDebug() << "infinite loop detected, aborting";
            QCoreApplication::quit();
            return; // TODO, report error better
        }
        // its not an error, its first layer of anti-bot
        auto body = reply_->readAll();
        QRegExp exp1("setTimeout\\(function\\(\\)\\{\n(.*)f.submit\\(\\);");
        int offset = exp1.indexIn(body);
        if (offset < 0) {
            qDebug() << "js not found";
            return;
        }
        QString match = exp1.capturedTexts().at(1);
        qDebug() << qPrintable(match);
        QString answer = defeatJSTest(match);

        QRegExp exp2("name=\"jschl_vc\" value=\"([^\"]*)\"");
        int offset2 = exp2.indexIn(body);
        if (offset2 < 0) {
            qDebug() << "vc not found";
            return;
        }

        QRegExp exp3("name=\"pass\" value=\"([^\"]*)\"");
        int offset3 = exp3.indexIn(body);
        if (offset3 < 0) {
            qDebug() << "pass not found";
            return;
        }

        QString var1 = exp2.capturedTexts().at(1);
        QString var2 = exp3.capturedTexts().at(1);

        final_answer = QUrl(QString("%1/cdn-cgi/l/chk_jschl?jschl_vc=%2&pass=%3&jschl_answer=%4").arg(kiss_->root_.toString()).arg(var1).arg(var2).arg(answer));
        reply_->deleteLater();
        reply_ = nullptr;
        delay.start(5000);
        done = true;
        //qDebug() << qPrintable(body);
        break;
    }
}

void KissParseRequest::send_answer() {
    qDebug() << "ding!";
    QNetworkRequest req(final_answer);
    //req.setHeader(QNetworkRequest::UserAgentHeader, user_agent);
    req.setRawHeader("Referer", qPrintable(absolute.toString()));
    req.setMaximumRedirectsAllowed(2);
    reply_ = kiss_->manager->get(req);
    connect(reply_, SIGNAL(finished()), this, SLOT(finished()));
}