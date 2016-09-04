#pragma once

#include <QObject>

#include "core.hpp"

class Tracer : public QObject {
Q_OBJECT
public:
    Tracer(chat::Core *core);
private slots:
    void onMessage(uint32_t friend_number, TOX_MESSAGE_TYPE type, QString message);
    void onLosslessPacket(uint32_t friend_number, QByteArray message);
    void onLossyPacket(uint32_t friend_number, QByteArray message);
};
