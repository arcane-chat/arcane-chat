#include <QDebug>

#include "tracer.hpp"

Tracer::Tracer(chat::Core *core) {
    connect(core,SIGNAL(onMessage(uint32_t,TOX_MESSAGE_TYPE,QString)),this,SLOT(onMessage(uint32_t,TOX_MESSAGE_TYPE,QString)));
    connect(core,SIGNAL(onLosslessPacket(uint32_t,QByteArray)),this,SLOT(onLosslessPacket(uint32_t,QByteArray)));
    connect(core,SIGNAL(onLossyPacket(uint32_t,QByteArray)),this,SLOT(onLossyPacket(uint32_t,QByteArray)));
}

void Tracer::onLosslessPacket(uint32_t friend_number, QByteArray message) {
    qDebug() << __func__ << friend_number << message.toHex();
}

void Tracer::onLossyPacket(uint32_t friend_number, QByteArray message) {
    qDebug() << __func__ << friend_number << message.toHex();
}

void Tracer::onMessage(uint32_t friend_number, TOX_MESSAGE_TYPE type, QString message) {
    qDebug() << __func__ << friend_number << type << message;
}
