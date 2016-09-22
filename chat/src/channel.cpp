#include "channel.hpp"

#include <sodium.h>

using namespace chat;

Channel::Channel() {
	self_joined_ = false;
}

void Channel::set_name(QString val) {
	name_ = val;
}

void Channel::add_member(Core *core) {
	self_joined_ = true;
	emit self_changed(true);
	emit members_changed();
}

void Channel::remove_member(Core *core) {
	self_joined_ = false;
	emit self_changed(false);
	emit members_changed();
}

Channel *Channel::create_new() {
    Channel *c = new Channel();
    unsigned char pubkey[crypto_sign_PUBLICKEYBYTES];
    unsigned char privkey[crypto_sign_SECRETKEYBYTES];
    crypto_sign_keypair(pubkey, privkey);
    c->set_keypair(QByteArray(reinterpret_cast<const char*>(pubkey), crypto_sign_PUBLICKEYBYTES),
        QByteArray(reinterpret_cast<const char*>(privkey), crypto_sign_SECRETKEYBYTES));
    return c;
}

void Channel::set_keypair(QByteArray pubkey, QByteArray privkey) {
    pubkey_ = pubkey;
    privkey_ = privkey;
}
