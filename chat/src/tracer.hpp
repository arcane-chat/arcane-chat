#pragma once

#include <QObject>

#include "core.hpp"

namespace chat {
    class Friend;
}
using namespace chat;
class Tracer : public QObject {
Q_OBJECT
public:
    Tracer(chat::Core *core);
private slots:
    void on_message(Friend *friend_number, bool type, QString message);
    void onLosslessPacket(uint32_t friend_number, QByteArray message);
    void onLossyPacket(uint32_t friend_number, QByteArray message);
};
