#pragma once

#include <vector>
#include <string>
#include <QObject>
#include "core.hpp"
#include "friend.hpp"

using namespace chat;

class Tracer : public QObject {
    Q_OBJECT
public:
    explicit Tracer(Core* core);
private Q_SLOTS:
    void on_message(Friend* friend_number, bool type, QString message);
    void on_lossless_packet(Friend* friend_number, QByteArray message);
    void on_lossy_packet(Friend* friend_number, QByteArray message);
};

//! FIXME: doc
namespace tox {
namespace utils {
//! Print a hexadecimal array.
QString to_hex(const std::vector<uint8_t>& bin);

//! Print a hexadecimal array.
QString to_hex(const uint8_t* bin_arr, size_t bin_size);

//! Parse a hexadecimal array.
QByteArray from_hex(const QString& hex);
}
}
