#include <iostream>

#include "core.hpp"
#include "utils.hpp"

using namespace chat;

static void MyFriendMessageCallback(Tox *tox, uint32_t friend_number,
                             TOX_MESSAGE_TYPE type, const uint8_t *message,
                             size_t length, void *user_data) {
    Core *core = (Core*) user_data;
    core->handleMessage(friend_number, type, QByteArray((const char*)message, length));
}

static void MyFriendLossyPacket(Tox *tox, uint32_t friend_number,
                         const uint8_t *data, size_t length,
                         void *user_data) {
    Core *core = (Core*) user_data;
    core->handleLossyPacket(friend_number, QByteArray((const char*)data, length));
}

static void MyFriendLosslessPacket(Tox *tox, uint32_t friend_number,
                            const uint8_t *data, size_t length,
                            void *user_data) {
    Core *core = (Core*) user_data;
    core->handleLosslessPacket(friend_number, QByteArray((const char*)data, length));
}

Core::Core(Tox *tox) : tox(tox) {
    tox_callback_friend_message(tox, MyFriendMessageCallback, this);
    tox_callback_friend_lossy_packet(tox, MyFriendLossyPacket, nullptr);
    tox_callback_friend_lossless_packet(tox, MyFriendLosslessPacket, nullptr);

    iterator.setSingleShot(true);
    connect(&iterator,SIGNAL(timeout()),this,SLOT(checkTox()));
    iterator.setInterval(tox_iteration_interval(tox));
    iterator.start();
}

void Core::checkTox() {
    tox_iterate(tox);
    // ^^^ will call the callback functions defined and registered
    iterator.setInterval(tox_iteration_interval(tox));
    iterator.start();
}

void Core::handleMessage(uint32_t friend_number, TOX_MESSAGE_TYPE type,
                QByteArray message) {
    QString text(message);
    emit onMessage(friend_number,type,text);
}

void Core::handleLosslessPacket(uint32_t friend_number, QByteArray message) {
    emit onLosslessPacket(friend_number,message);
}

void Core::handleLossyPacket(uint32_t friend_number, QByteArray message) {
    emit onLossyPacket(friend_number,message);
}
