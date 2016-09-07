#include <vector>
#include <string>
#include <cstdint>
#include <boost/algorithm/hex.hpp>
#include <QDebug>

#include "utils.hpp"

Tracer::Tracer(chat::Core* core) {
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
std::string to_hex(const std::vector<uint8_t>& bin) {
    std::string out;
    out.resize(bin.size() * 2);
    boost::algorithm::hex(bin.begin(), bin.end(), out.begin());
    return out;
}

std::string to_hex(const uint8_t* bin_arr, size_t bin_size) {
    const std::vector<uint8_t> vec{bin_arr, bin_arr + bin_size};
    return to_hex(vec);
}

std::vector<uint8_t> from_hex(const std::string& hex) {
    std::vector<uint8_t> out;
    out.resize(hex.size() / 2);
    boost::algorithm::unhex(hex.begin(), hex.end(), out.begin());
    return out;
}
}
}
