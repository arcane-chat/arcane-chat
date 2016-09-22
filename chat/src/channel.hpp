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

    QString name() const { return name_; }
    void set_name(QString val);
    Q_PROPERTY(QString name READ name WRITE set_name);

    int rowid() const { return rowid_; }
    void set_rowid(int val) { rowid_ = val; }
    Q_PROPERTY(int rowid READ rowid WRITE set_rowid);
signals:
	void members_changed();
	void self_changed(bool joined);
	void friend_changed(chat::Friend *fr, bool joined);
private:
    QString name_;
    int rowid_;
    bool self_joined_;
};
}
