#pragma once

#include "enums.hpp"

#include <cstdint>
#include <string>
#include <vector>
#include <QByteArray>

struct Tox_Options;

namespace tox {
    class options {
    private:
        Tox_Options* underlying_;
        std::string proxy_host_;
        std::vector<uint8_t> savedata_data_;
        QByteArray savedata_data2_;

    public:
        options();
        ~options();

        void set_ipv6_enabled(bool enabled);
        void set_udp_enabled(bool enabled);

        void set_proxy_type(tox::ProxyType type);
        void set_proxy_host(std::string host);
        void set_proxy_port(uint16_t port);

        void set_start_port(uint16_t port);
        void set_end_port(uint16_t port);
        void set_tcp_port(uint16_t port);

        void set_savedata_type(tox::SaveDataType type);
        void set_savedata_data(std::vector<uint8_t> data);
        void set_savedata_data(QByteArray data);

        inline struct Tox_Options* get_underlying() {
            return underlying_;
        }
    };
}
