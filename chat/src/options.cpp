#include "options.hpp"

#include <tox/tox.h>

#include <QDebug>

namespace tox {
    options::options() : underlying_(tox_options_new(nullptr)) {}

    options::~options() { tox_options_free(underlying_); }

    void options::set_ipv6_enabled(bool enabled) {
        underlying_->ipv6_enabled = enabled;
    }

    void options::set_udp_enabled(bool enabled) {
        underlying_->udp_enabled = enabled;
    }

    void options::set_proxy_type(tox::ProxyType type) {
        underlying_->proxy_type = static_cast<TOX_PROXY_TYPE>(type);
    }

    void options::set_proxy_host(std::string host) {
        proxy_host_ = host;
        underlying_->proxy_host = proxy_host_.c_str();
    }

    void options::set_proxy_port(uint16_t port) {
        underlying_->proxy_port = port;
    }

    void options::set_start_port(uint16_t port) {
        underlying_->start_port = port;
    }

    void options::set_end_port(uint16_t port) {
        underlying_->end_port = port;
    }

    void options::set_tcp_port(uint16_t port) {
        underlying_->tcp_port = port;
    }

    void options::set_savedata_type(tox::SaveDataType type) {
        underlying_->savedata_type = static_cast<TOX_SAVEDATA_TYPE>(type);
    }

    void options::set_savedata_data(std::vector<uint8_t> data) {
        savedata_data_ = data;
        underlying_->savedata_data = savedata_data_.data();
        underlying_->savedata_length = savedata_data_.size();
    }

    void options::set_savedata_data(QByteArray data) {
        savedata_data2_ = data;
        underlying_->savedata_data = reinterpret_cast<uint8_t*>(savedata_data2_.data());
        underlying_->savedata_length = savedata_data2_.size();
    }
}
