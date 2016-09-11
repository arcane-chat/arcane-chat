#include <vector>
#include <string>
#include <cstdint>
#include <QDebug>

#include "utils.hpp"

Tracer::Tracer(chat::Core* core) : QObject(core) {
    connect(core, SIGNAL(on_message(Friend*, bool, QString)), this,
            SLOT(on_message(Friend*, bool, QString)));
    connect(core, SIGNAL(on_lossless_packet(Friend*, QByteArray)), this,
            SLOT(on_lossless_packet(Friend*, QByteArray)));
    connect(core, SIGNAL(on_lossy_packet(Friend*, QByteArray)), this,
            SLOT(on_lossy_packet(Friend*, QByteArray)));
}

void Tracer::on_message(Friend* friend_number, bool type, QString message) {
    qDebug() << __func__ << friend_number << type << message;
}

void Tracer::on_lossless_packet(Friend* friend_number, QByteArray message) {
    qDebug() << __func__ << friend_number << message.toHex();
}

void Tracer::on_lossy_packet(Friend* friend_number, QByteArray message) {
    qDebug() << __func__ << friend_number << message.toHex();
}

namespace tox {
namespace utils {
QString to_hex(const QByteArray bin) {
    return bin.toHex();
}

QString to_hex(const uint8_t* bin_arr, size_t bin_size) {
    return QByteArray(reinterpret_cast<const char*>(bin_arr), bin_size).toHex();
}

QByteArray from_hex(const QString& str) {
    return QByteArray::fromHex(QByteArray(qPrintable(str)));
}
}
}
