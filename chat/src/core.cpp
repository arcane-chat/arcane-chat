#include <iostream>
#include <cassert>
#include <QDebug>
#include <sstream>

#include "core.hpp"
#include "utils.hpp"

using namespace chat;

namespace {
    QByteArray make_qba(const uint8_t* data, size_t length) {
        return QByteArray(reinterpret_cast<const char*>(data), length);
    }

    void friend_message_callback(tox::clib::Tox* tox,
                                 uint32_t friend_number,
                                 tox::clib::TOX_MESSAGE_TYPE type,
                                 const uint8_t* message,
                                 size_t length,
                                 void* user_data) {
        Q_UNUSED(tox);
        Core* core = (Core*) user_data;
        core->handle_message(friend_number, type, make_qba(message, length));
    }

    void friend_lossy_packet(tox::clib::Tox* tox,
                             uint32_t friend_number,
                             const uint8_t* data,
                             size_t length,
                             void* user_data) {
        Q_UNUSED(tox);
        Core* core = (Core*) user_data;
        core->handle_lossy_packet(friend_number, make_qba(data, length));
    }

    void friend_lossless_packet(tox::clib::Tox* tox,
                                uint32_t friend_number,
                                const uint8_t* data,
                                size_t length,
                                void* user_data) {
        Q_UNUSED(tox);
        Core* core = (Core*) user_data;
        core->handle_lossless_packet(friend_number,
                                     make_qba(data, length));
    }

    void friend_typing(tox::clib::Tox* tox,
                       uint32_t friend_number,
                       bool is_typing,
                       void* user_data) {
        Q_UNUSED(tox);
        qDebug() << friend_number << is_typing << "is typing";
    }

    void friend_connection_update(tox::clib::Tox* tox,
                                  uint32_t friend_number,
                                  tox::clib::TOX_CONNECTION connection_status,
                                  void* user_data) {
        Q_UNUSED(tox);
        Core* core = (Core*) user_data;
        core->handle_friend_connection_update(friend_number, connection_status);
        std::cout << __func__
                  << " " << friend_number
                  << " " << connection_status
                  << "\n";
    }

} // namespace

Core::Core(tox::clib::Tox* tox) : tox(tox) {
    tox::clib::tox_callback_friend_typing(tox, friend_typing, this);
    tox::clib::tox_callback_friend_message(tox, friend_message_callback, this);
    tox::clib::tox_callback_friend_lossy_packet(tox, friend_lossy_packet, this);
    tox::clib::tox_callback_friend_lossless_packet(tox, friend_lossless_packet, this);
    tox::clib::tox_callback_friend_connection_status(tox, friend_connection_update, this);

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
    tox::clib::TOX_ERR_FRIEND_GET_PUBLIC_KEY error;
    tox::clib::TOX_ERR_FRIEND_QUERY error2;

    tox_self_get_friend_list(tox, friends);

    qDebug() << "I have " << count << " friends!";

    for(int i = 0; i < count; i++) {
        tox_friend_get_public_key(tox, friends[i], pubkey, &error);
        assert(error == tox::clib::TOX_ERR_FRIEND_GET_PUBLIC_KEY_OK);
        size_t size = tox_friend_get_name_size(tox, friends[i], &error2);
        assert(error2 == tox::clib::TOX_ERR_FRIEND_QUERY_OK);
        uint8_t name[size];
        tox_friend_get_name(tox, friends[i], name, &error2);
        assert(error2 == tox::clib::TOX_ERR_FRIEND_QUERY_OK);

        tox::clib::TOX_CONNECTION link =
            tox_friend_get_connection_status(tox, friends[i], nullptr);

        chat::Link newlink;

        switch(link) {
        case tox::clib::TOX_CONNECTION_NONE: newlink = chat::Link::None; break;
        case tox::clib::TOX_CONNECTION_TCP: newlink = chat::Link::Tcp; break;
        case tox::clib::TOX_CONNECTION_UDP: newlink = chat::Link::Udp; break;
        default: Q_ASSERT(false); return;
        }

        Friend* f = new Friend(
            friends[i], QByteArray((const char*) pubkey, TOX_PUBLIC_KEY_SIZE),
            QString(QByteArray((const char*) name, size)), newlink);
        this->friends.append(f);
    }
}

void Core::check_tox() {
    tox_iterate(tox);
    // ^^^ will call the callback functions defined and registered
    iterator.setInterval(tox_iteration_interval(tox));
    iterator.start();
}

void Core::handle_message(uint32_t friend_number,
                          tox::clib::TOX_MESSAGE_TYPE type,
                          QByteArray message) {
    QString text(message);
    bool action = (type == tox::clib::TOX_MESSAGE_TYPE_ACTION);
    for(chat::Friend* f : friends) {
        if(f->friend_number == friend_number) {
            emit on_message(f, action, text);
            f->new_message(action, message);
        }
    }
}

void Core::handle_lossless_packet(uint32_t friend_number, QByteArray message) {
    emit on_lossless_packet(friend_number, message);
}

void Core::handle_lossy_packet(uint32_t friend_number, QByteArray message) {
    emit on_lossy_packet(friend_number, message);
}

void Core::handle_friend_connection_update(uint32_t friend_number,
                                           tox::clib::TOX_CONNECTION link) {
    chat::Link newlink;

    switch(link) {
    case tox::clib::TOX_CONNECTION_NONE: newlink = chat::Link::none; break;
    case tox::clib::TOX_CONNECTION_TCP: newlink = chat::Link::tcp; break;
    case tox::clib::TOX_CONNECTION_UDP: newlink = chat::Link::udp; break;
    default: Q_ASSERT(false); return;
    }

    for(chat::Friend* f : friends) {
        if(f->friend_number == friend_number) {
            f->set_connection(newlink);
        }
    }
}

void Core::send_message(uint32_t friend_number, bool action, QString message) {
    tox::clib::TOX_ERR_FRIEND_SEND_MESSAGE error;
    QByteArray bytes = message.toUtf8();
    uint8_t* msg = (uint8_t*) bytes.data();
    size_t size = bytes.size();
    tox::clib::tox_friend_send_message(tox, friend_number,
                                       tox::clib::TOX_MESSAGE_TYPE_NORMAL,
                                       msg, size, &error);
}
