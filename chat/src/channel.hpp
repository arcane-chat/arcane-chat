#pragma once

#include <QObject>

namespace chat {

class Core;
class Friend;

class Channel : public QObject {
Q_OBJECT
public:
    explicit Channel();
    void add_member(Core *core);
    void remove_member(Core *core);
    static Channel *create_new();
    void set_keypair(QByteArray pubkey, QByteArray privkey);

    QString name() const { return name_; }
    void set_name(QString val);
    Q_PROPERTY(QString name READ name WRITE set_name);

    QByteArray pubkey() { return pubkey_; }
    void set_pubkey(QByteArray in) { pubkey_ = in; }
    QByteArray privkey() { return privkey_; }
    void set_privkey(QByteArray in) { privkey_ = in; }
signals:
	void members_changed();
	void self_changed(bool joined);
	void friend_changed(chat::Friend *fr, bool joined);
private:
    QString name_;
    bool self_joined_;
    QByteArray pubkey_, privkey_;
};
}
