#include <iostream>
#include <cassert>
#include <QDebug>
#include <sstream>

#include "core.hpp"
#include "utils.hpp"

using namespace chat;

static void MyFriendMessageCallback(Tox* tox,
                                    uint32_t friend_number,
                                    TOX_MESSAGE_TYPE type,
                                    const uint8_t* message,
                                    size_t length,
                                    void* user_data) {
    Core* core = (Core*) user_data;
    core->handleMessage(friend_number, type,
                        QByteArray((const char*) message, length));
}

static void MyFriendLossyPacket(Tox* tox,
                                uint32_t friend_number,
                                const uint8_t* data,
                                size_t length,
                                void* user_data) {
    Core* core = (Core*) user_data;
    core->handleLossyPacket(friend_number,
                            QByteArray((const char*) data, length));
}

static void MyFriendLosslessPacket(Tox* tox,
                                   uint32_t friend_number,
                                   const uint8_t* data,
                                   size_t length,
                                   void* user_data) {
    Core* core = (Core*) user_data;
    core->handleLosslessPacket(friend_number,
                               QByteArray((const char*) data, length));
}

static void MyFriendTyping(Tox* tox,
                           uint32_t friend_number,
                           bool is_typing,
                           void* user_data) {
    qDebug() << friend_number << is_typing << "is typing";
}

static void FriendConnectionUpdate(Tox* tox,
                                   uint32_t friend_number,
                                   TOX_CONNECTION connection_status,
                                   void* user_data) {
    Core* core = (Core*) user_data;
    core->handle_friend_connection_update(friend_number, connection_status);
    std::cout << __func__ << " " << friend_number << " " << connection_status
              << "\n";
}

Core::Core(Tox* tox) : tox(tox) {
    tox_callback_friend_typing(tox, MyFriendTyping, this);
    tox_callback_friend_message(tox, MyFriendMessageCallback, this);
    tox_callback_friend_lossy_packet(tox, MyFriendLossyPacket, this);
    tox_callback_friend_lossless_packet(tox, MyFriendLosslessPacket, this);
    tox_callback_friend_connection_status(tox, FriendConnectionUpdate, this);

    std::string username = ({
        std::stringstream ss;
        ss << "fuspr-" << rand();
        ss.str();
    });
    tox_self_set_name(tox, reinterpret_cast<const uint8_t*>(username.c_str()),
                      username.size(), nullptr);

    this->username = username.c_str();

    iterator.setSingleShot(true);
    connect(&iterator, SIGNAL(timeout()), this, SLOT(checkTox()));
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

        chat::Link newlink;

        switch(link) {
        case TOX_CONNECTION_NONE: newlink = chat::Link::None; break;
        case TOX_CONNECTION_TCP: newlink = chat::Link::Tcp; break;
        case TOX_CONNECTION_UDP: newlink = chat::Link::Udp; break;
        }

        Friend* f = new Friend(
            friends[i], QByteArray((const char*) pubkey, TOX_PUBLIC_KEY_SIZE),
            QString(QByteArray((const char*) name, size)), newlink);
        this->friends.append(f);
    }
}

void Core::checkTox() {
    tox_iterate(tox);
    // ^^^ will call the callback functions defined and registered
    iterator.setInterval(tox_iteration_interval(tox));
    iterator.start();
}

void Core::handleMessage(uint32_t friend_number,
                         TOX_MESSAGE_TYPE type,
                         QByteArray message) {
    QString text(message);
    bool action = type == TOX_MESSAGE_TYPE_ACTION;
    for(chat::Friend* f : friends) {
        if(f->friend_number == friend_number) {
            emit on_message(f, type == TOX_MESSAGE_TYPE_ACTION, text);
            f->new_message(action, message);
        }
    }
}

void Core::handleLosslessPacket(uint32_t friend_number, QByteArray message) {
    emit onLosslessPacket(friend_number, message);
}

void Core::handleLossyPacket(uint32_t friend_number, QByteArray message) {
    emit onLossyPacket(friend_number, message);
}

void Core::handle_friend_connection_update(uint32_t friend_number,
                                           TOX_CONNECTION link) {
    chat::Link newlink;

    switch(link) {
    case TOX_CONNECTION_NONE: newlink = chat::Link::None; break;
    case TOX_CONNECTION_TCP: newlink = chat::Link::Tcp; break;
    case TOX_CONNECTION_UDP: newlink = chat::Link::Udp; break;
    }

    foreach(chat::Friend* f, friends) {
        if(f->friend_number == friend_number) {
            f->set_connection(newlink);
        }
    }
}

void Core::send_message(uint32_t friend_number, bool action, QString message) {
    TOX_ERR_FRIEND_SEND_MESSAGE error;
    QByteArray bytes = message.toUtf8();
    uint8_t* msg = (uint8_t*) bytes.data();
    size_t size = bytes.size();
    tox_friend_send_message(tox, friend_number, TOX_MESSAGE_TYPE_NORMAL, msg,
                            size, &error);
}
