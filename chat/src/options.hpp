#pragma once

#include "enums.hpp"

namespace tox {
    class options {
    private:
        struct Tox_Options* underlying_;
        std::string proxy_host_;
        std::vector<uint8_t> savedata_data_;
    public:
        inline options() : underlying_(tox_options_new(nullptr)) {
        }

        ~options() {
            tox_options_free(underlying_);
        }

        void set_ipv6_enabled(bool enabled) {
            underlying_->ipv6_enabled = enabled;
        }

        void set_udp_enabled(bool enabled) {
            underlying_->udp_enabled = enabled;
        }

        void set_proxy_type(proxy_type type) {
            underlying_->proxy_type = static_cast<TOX_PROXY_TYPE>(type);
        }

        void set_proxy_host(std::string host) {
            proxy_host_ = host;
            underlying_->proxy_host = proxy_host_.c_str();
        }

        void set_proxy_port(uint16_t port) { underlying_->proxy_port = port; }
        void set_start_port(uint16_t port) { underlying_->start_port = port; }
        void set_end_port  (uint16_t port) { underlying_->end_port   = port; }
        void set_tcp_port  (uint16_t port) { underlying_->tcp_port   = port; }

        void set_savedata_type(savedata_type type) {
            underlying_->savedata_type = static_cast<TOX_SAVEDATA_TYPE>(type);
        }

        void set_savedata_data(std::vector<uint8_t> data) {
            savedata_data_ = data;
            underlying_->savedata_data = savedata_data_.data();
            underlying_->savedata_length = savedata_data_.size();
        }
    };
}
