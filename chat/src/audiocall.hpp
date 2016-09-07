#pragma once

#define QT_NO_SIGNALS_SLOTS_KEYWORDS

#include <glibmm.h>
#include <giomm/outputstream.h>

#include <QObject>
#include "toxoutputstream.hpp"

class AudioCall : public QObject
{
    Q_OBJECT
public:
    explicit AudioCall(QObject *parent = 0);
    void create_instance();
    void create_pipeline();
Q_SIGNALS:

public Q_SLOTS:
private:
    ToxOutputStream *outputstream;
    Glib::RefPtr<Gio::OutputStream> reference;
};
