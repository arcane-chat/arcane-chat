#include <iostream>

#include "core.hpp"

using namespace chat;

void MyFriendMessageCallback(Tox *tox, uint32_t friend_number,
                             TOX_MESSAGE_TYPE type, const uint8_t *message,
                             size_t length, void *user_data) {
    Core *core = (Core*) user_data;
    std::cout << "message: \"" << message << "\"\n";
    core->handleMessage(friend_number, type, message, length);
}

Core::Core(Tox *tox) : tox(tox) {
    tox_callback_friend_message(tox, MyFriendMessageCallback, this);

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
                const unsigned char *message, size_t length) {
    QByteArray raw((const char *)message,length);
    QString text(raw);
    emit onMessage(friend_number,type,text);
}
