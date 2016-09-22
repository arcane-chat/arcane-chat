#include "channel.hpp"

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
