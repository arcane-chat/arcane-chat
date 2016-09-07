#include <iostream>
#include <cassert>
#include <QDebug>
#include <sstream>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>

#include "core.hpp"
#include "utils.hpp"

using namespace chat;

namespace {
    constexpr const char* bootstrap_address = "23.226.230.47";

    constexpr int bootstrap_port = 33445;

    constexpr const char* bootstrap_key =
        "A09162D68618E742FFBCA1C2C70385E6679604B2D80EA6E84AD0996A1AC8A074";

    void save_state(Tox* tox) {
        size_t size = tox_get_savedata_size(tox);
        uint8_t* savedata = new uint8_t[size];
        tox_get_savedata(tox, savedata);
        int fd = open("/tmp/savedata", O_TRUNC | O_WRONLY | O_CREAT, 0644);
        assert(fd);
        ssize_t written = write(fd, savedata, size);
        assert(written > 0);
        close(fd);
    }

    tox::LinkType convert_link_type(TOX_CONNECTION link_type) {
        switch(link_type) {
        case TOX_CONNECTION_NONE: return tox::LinkType::none;
        case TOX_CONNECTION_TCP:  return tox::LinkType::tcp;
        case TOX_CONNECTION_UDP:  return tox::LinkType::udp;
        default: Q_ASSERT(false); return tox::LinkType::none;
        }
    }

    tox::MessageType convert_message_type(TOX_MESSAGE_TYPE message_type) {
        switch(message_type) {
        case TOX_MESSAGE_TYPE_NORMAL: return tox::MessageType::normal;
        case TOX_MESSAGE_TYPE_ACTION: return tox::MessageType::action;
        default: Q_ASSERT(false);     return tox::MessageType::normal;
        }
    }

    QByteArray make_qba(const uint8_t* data, size_t length) {
        return QByteArray(reinterpret_cast<const char*>(data), length);
    }

    void friend_message_callback(Tox* tox,
                                 uint32_t friend_number,
                                 TOX_MESSAGE_TYPE type,
                                 const uint8_t* message,
                                 size_t length,
                                 void* user_data) {
        Q_UNUSED(tox);
        Core* core = (Core*) user_data;
        core->handle_message(friend_number, convert_message_type(type),
                             make_qba(message, length));
    }

    void friend_lossy_packet(Tox* tox,
                             uint32_t friend_number,
                             const uint8_t* data,
                             size_t length,
                             void* user_data) {
        Q_UNUSED(tox);
        Core* core = (Core*) user_data;
        core->handle_lossy_packet(friend_number, make_qba(data, length));
    }

    void friend_lossless_packet(Tox* tox,
                                uint32_t friend_number,
                                const uint8_t* data,
                                size_t length,
                                void* user_data) {
        Q_UNUSED(tox);
        Core* core = (Core*) user_data;
        core->handle_lossless_packet(friend_number,
                                     make_qba(data, length));
    }

    void friend_typing(Tox* tox,
                       uint32_t friend_number,
                       bool is_typing,
                       void* user_data) {
        Q_UNUSED(tox);
        qDebug() << friend_number << is_typing << "is typing";
    }

    void friend_request(Tox* tox,
                        const uint8_t* public_key,
                        const uint8_t* message,
                        size_t length,
                        void* user_data) {
        TOX_ERR_FRIEND_ADD error;
        // Accept any friend request sent to us
        tox_friend_add_norequest(tox, public_key, &error);
        switch(error) {
        case TOX_ERR_FRIEND_ADD_OK:
            break;
        case TOX_ERR_FRIEND_ADD_ALREADY_SENT:
            qDebug() << "already sent";
            break;
        case TOX_ERR_FRIEND_ADD_BAD_CHECKSUM:
            qDebug() << "crc error";
            break;
        default:
            qDebug() << "error code: " << error;
        }
        save_state(tox);
    }

    void friend_connection_status(Tox* tox,
                                  uint32_t friend_number,
                                  TOX_CONNECTION connection_status,
                                  void* user_data) {
        Q_UNUSED(tox);
        Core* core = (Core*) user_data;
        tox::LinkType link_type = convert_link_type(connection_status);
        core->handle_friend_connection_status(friend_number, link_type);
        std::cout << __func__
                  << " " << friend_number
                  << " " << connection_status
                  << "\n";
    }

    void self_connection_status(Tox* tox,
                                TOX_CONNECTION connection_status,
                                void* user_data) {
        Q_UNUSED(user_data);
        uint8_t toxid[TOX_ADDRESS_SIZE];
        tox_self_get_address(tox, toxid);
        std::string tox_printable_id = tox::utils::to_hex(toxid, TOX_ADDRESS_SIZE);

        const char* msg = nullptr;

        switch(connection_status) {
        case TOX_CONNECTION_NONE:
            msg = "offline";
            std::cout << "connection lost\n";
            break;
        case TOX_CONNECTION_TCP:
            msg = "connected via tcp";
            std::cout << "tcp connection established\n";
            break;
        case TOX_CONNECTION_UDP:
            msg = "connected via udp";
            std::cout << "udp connection established\n";
            break;
        }

        if(msg != nullptr) {
            std::cout << "status = " << msg << ", "
                      << "id = " << tox_printable_id << "\n";
        }

        save_state(tox);
        fflush(stdout);
    }
} // namespace

Core::Core() : tox(nullptr) {
    struct Tox_Options* opts = tox_options_new(nullptr);
    opts->start_port = 33445;
    opts->end_port = 33445 + 100;
    int oldstate = open("/tmp/savedata", O_RDONLY);
    if(oldstate >= 0) {
        struct stat info;
        fstat(oldstate, &info);
        uint8_t* temp = new uint8_t[info.st_size];
        ssize_t size = read(oldstate, temp, info.st_size);
        close(oldstate);
        assert(size == info.st_size);
        opts->savedata_type = TOX_SAVEDATA_TYPE_TOX_SAVE;
        opts->savedata_data = temp;
        opts->savedata_length = size;
    }
    TOX_ERR_NEW new_error;
    tox = tox_new(opts, &new_error);
    if(!tox) {
        opts->ipv6_enabled = false;
        tox = tox_new(opts, &new_error);
    }
    if(opts->savedata_data) {
        delete opts->savedata_data;
    }
    tox_options_free(opts);
    opts = 0;
    uint8_t toxid[TOX_ADDRESS_SIZE];
    tox_self_get_address(tox, toxid);
    std::cout << "my id is "
              << tox::utils::to_hex(toxid, TOX_ADDRESS_SIZE)
              << "\n";
    std::vector<uint8_t> bootstrap_pub_key { tox::utils::from_hex(bootstrap_key) };
    tox_bootstrap(tox, bootstrap_address, bootstrap_port,
                  bootstrap_pub_key.data(), nullptr);

    tox_callback_friend_typing(tox, friend_typing, this);
    tox_callback_friend_request(tox, friend_request, this);
    tox_callback_friend_message(tox, friend_message_callback, this);
    tox_callback_friend_lossy_packet(tox, friend_lossy_packet, this);
    tox_callback_friend_lossless_packet(tox, friend_lossless_packet, this);
    tox_callback_friend_connection_status(tox, friend_connection_status, this);
    tox_callback_self_connection_status(tox, self_connection_status, this);

    std::string username = ({
            std::stringstream ss;
            ss << "fuspr-" << rand();
            ss.str();
        });
    tox_self_set_name(tox, reinterpret_cast<const uint8_t*>(username.c_str()),
                      username.size(), nullptr);

    this->username = username.c_str();

    iterator.setSingleShot(true);
    connect(&iterator, SIGNAL(timeout()), this, SLOT(check_tox()));
    iterator.setInterval(tox_iteration_interval(tox));
    iterator.start();

    int count = tox_self_get_friend_list_size(tox);
    uint32_t friends[count];
    uint8_t pubkey[TOX_PUBLIC_KEY_SIZE];
    TOX_ERR_FRIEND_GET_PUBLIC_KEY error;
    TOX_ERR_FRIEND_QUERY error2;

    tox_self_get_friend_list(tox, friends);

    qDebug() << "I have " << count << " friends!";

    for(int i = 0; i < count; i++) {
        tox_friend_get_public_key(tox, friends[i], pubkey, &error);
        assert(error == TOX_ERR_FRIEND_GET_PUBLIC_KEY_OK);
        size_t size = tox_friend_get_name_size(tox, friends[i], &error2);
        assert(error2 == TOX_ERR_FRIEND_QUERY_OK);
        uint8_t name[size];
        tox_friend_get_name(tox, friends[i], name, &error2);
        assert(error2 == TOX_ERR_FRIEND_QUERY_OK);

        TOX_CONNECTION link =
            tox_friend_get_connection_status(tox, friends[i], nullptr);

        tox::LinkType newlink = convert_link_type(link);

        Friend* f = new Friend {
            friends[i],
            make_qba(pubkey, TOX_PUBLIC_KEY_SIZE),
            QString(make_qba(name, size)),
            newlink
        };

        this->friends.append(f);
    }
}

Core::~Core() {
    save_state(tox);
    tox_kill(tox);
}

void Core::check_tox() {
    tox_iterate(tox);
    // ^^^ will call the callback functions defined and registered
    iterator.setInterval(tox_iteration_interval(tox));
    iterator.start();
}

void Core::handle_message(uint32_t friend_number,
                          tox::MessageType type,
                          QByteArray message) {
    QString text(message);
    bool is_action = (type == tox::MessageType::action);
    for(chat::Friend* f : friends) {
        if(f->friend_number == friend_number) {
            emit on_message(f, is_action, text);
            f->new_message(is_action, message);
        }
    }
}

void Core::handle_lossless_packet(uint32_t friend_number, QByteArray message) {
    emit on_lossless_packet(friend_number, message);
}

void Core::handle_lossy_packet(uint32_t friend_number, QByteArray message) {
    emit on_lossy_packet(friend_number, message);
}

void Core::handle_friend_connection_status(uint32_t friend_number,
                                           tox::LinkType link) {
    for(chat::Friend* f : friends) {
        if(f->friend_number == friend_number) {
            f->set_connection(link);
        }
    }
}

void Core::send_message(uint32_t friend_number, bool action, QString message) {
    TOX_ERR_FRIEND_SEND_MESSAGE error;
    QByteArray bytes = message.toUtf8();
    uint8_t* msg = (uint8_t*) bytes.data();
    size_t size = bytes.size();
    tox_friend_send_message(tox, friend_number,
                            TOX_MESSAGE_TYPE_NORMAL,
                            msg, size, &error);
}
