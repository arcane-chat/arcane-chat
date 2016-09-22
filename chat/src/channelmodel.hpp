#pragma once

#include <QAbstractItemModel>
#include <QList>

#include "friend.hpp"
#include "channel.hpp"

namespace gui {

enum class NodeType { Root, LegacyFolder, LegacyFriend, ChannelFolder, ChannelNode,
    MemberNode };

class Node : public QObject {
    Q_OBJECT
public:
    Node(NodeType type, Node* parent) : type(type), parent(parent) {}
    virtual QVariant data();

    NodeType type;
    Node* parent;
    QList<Node*> children;

Q_SIGNALS:
    void changed(Node* self);
protected slots:
    void simple_change();
};

//! a legacy friend node
class FriendNode : public Node {
    Q_OBJECT
public:
    FriendNode(Node* parent, chat::Friend* f);
    virtual QVariant data();

    chat::Friend* f;
private Q_SLOTS:
    void connection_changed(tox::LinkType old_state, tox::LinkType new_state);
    void message(bool action, QByteArray message);
};

//! a single channel
class ChannelNode : public Node {
Q_OBJECT
public:
    explicit ChannelNode(Node *parent, chat::Channel *c);
    virtual QVariant data();
    chat::Channel *channel() { return channel_; };
private slots:
    void self_changed(bool joined);
private:
    chat::Channel *channel_;
};

//! a user in a channel
class MemberNode : public Node {
public:
    //! the local local node is a member
    explicit MemberNode(ChannelNode *parent);
    //! a friend is a member of the channel
    explicit MemberNode(ChannelNode *parent, chat::Friend *fr);
    virtual QVariant data();
private:
    //! if null, this node is for the local instance
    chat::Friend *fr_;
};

class ChannelModel : public QAbstractItemModel {
    Q_OBJECT
public:
    explicit ChannelModel(chat::Core *core);
    QModelIndex index(int row, int column, const QModelIndex& parent) const;
    QModelIndex parent(const QModelIndex& child) const;
    int rowCount(const QModelIndex& parent) const;
    int columnCount(const QModelIndex& parent) const;
    QVariant data(const QModelIndex& index, int role) const;
    Node* getNode(const QModelIndex& index) const;

    Node* root;

private Q_SLOTS:
    void node_changed(Node*);
    void on_new_friend(chat::Friend* fr);
private:
    Node *legacyFolder, *channelFolder;
};

}
