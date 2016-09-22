#include "channelmodel.hpp"
#include "core.hpp"

#include <QDebug>
#include <cassert>

using namespace gui;
using namespace chat;

ChannelModel::ChannelModel(Core *core) {
    root = new Node(NodeType::Root, nullptr);
    legacyFolder = new Node(NodeType::LegacyFolder, root);
    root->children.append(legacyFolder);

    channelFolder = new Node(NodeType::ChannelFolder, root);
    root->children.append(channelFolder);

    auto friends = core->friends();

    for(Friend* f : friends) {
        FriendNode* t = new FriendNode(legacyFolder, f);
        connect(t, SIGNAL(changed(Node*) ), this, SLOT(node_changed(Node*) ));
        legacyFolder->children.append(t);
    }

    connect(core, &Core::on_new_friend, this, &ChannelModel::on_new_friend);

    auto channels = core->channels();
    for (Channel *c : channels) {
        ChannelNode *t = new ChannelNode(channelFolder, c);
        connect(t, &Node::changed, this, &ChannelModel::node_changed);
        channelFolder->children.append(t);
    }
}

void ChannelModel::on_new_friend(Friend *fr) {
    FriendNode *t = new FriendNode(legacyFolder, fr);
    connect(t, &FriendNode::changed, this, &ChannelModel::node_changed);
    legacyFolder->children.append(t);
    // TODO, inform qt that the child count changed
}

int ChannelModel::rowCount(const QModelIndex& parent) const {
    return getNode(parent)->children.count();
}

QModelIndex
ChannelModel::index(int row, int column, const QModelIndex& parent) const {
    Node* node = getNode(parent);
    assert(row < node->children.count());
    Node* child = node->children[row];
    return createIndex(row, column, child);
}

QModelIndex ChannelModel::parent(const QModelIndex& child) const {
    Node* node = getNode(child);
    Node* parent = node->parent;
    Node* grandparent = parent->parent;
    if(grandparent) {
        int row = grandparent->children.indexOf(parent);
        return createIndex(row, 1, parent);
    } else {
        return QModelIndex();
    }
}

int ChannelModel::columnCount(const QModelIndex&) const { return 1; }

QVariant ChannelModel::data(const QModelIndex& index, int role) const {
    Node* node = getNode(index);
    if((role == Qt::DisplayRole) && (index.column() == 0)) {
        return node->data();
    } else {
        return QVariant();
    }
}

Node* ChannelModel::getNode(const QModelIndex& index) const {
    if(index.internalPointer()) {
        return reinterpret_cast<Node*>(index.internalPointer());
    } else {
        return root;
    }
}

QVariant Node::data() {
    switch (type) {
    case NodeType::LegacyFolder:
        return "Legacy Clients";
    case NodeType::ChannelFolder:
        return "My Channels";
    default:
        return "data!";
    }
}

QVariant FriendNode::data() {
    QString state;
    switch(f->connection) {
    case tox::LinkType::none: state = "offline"; break;
    case tox::LinkType::tcp: state = "tcp"; break;
    case tox::LinkType::udp: state = "udp"; break;
    }
    if (f->connection == tox::LinkType::none) {
        return QString("%1 , %2").arg(state).arg(f->get_username());
    } else {
        return QString("%1 , %2 rtt: %3 offset: %4").arg(state).arg(f->get_username()).arg(f->rtt.toString()).arg(f->offset.toString());
    }

}

FriendNode::FriendNode(Node* parent, chat::Friend* f)
    : Node(NodeType::LegacyFriend, parent), f(f) {
    connect(f, SIGNAL(connection_changed(tox::LinkType, tox::LinkType)), this,
            SLOT(connection_changed(tox::LinkType, tox::LinkType)));
    connect(f, SIGNAL(message(bool, QByteArray)), this,
            SLOT(message(bool, QByteArray)));
    connect(f, SIGNAL(latency_update()), this, SLOT(simple_change()));
    connect(f, SIGNAL(username_changed()), this, SLOT(simple_change()));
}

void FriendNode::connection_changed(tox::LinkType old_state,
                                    tox::LinkType new_state) {
    emit changed(this);
}

void FriendNode::message(bool action, QByteArray message) {
}

void Node::simple_change() {
    emit changed(this);
}

void ChannelModel::node_changed(Node* node) {
    assert(node->parent);
    int row = node->parent->children.indexOf(node);
    QModelIndex index = createIndex(row, 0, node);
    emit dataChanged(index, index);
}

ChannelNode::ChannelNode(Node *parent, Channel *c)
    : Node(NodeType::ChannelNode, parent), channel_(c) {
    connect(c, &Channel::members_changed, this, &ChannelNode::simple_change);
    connect(c, &Channel::self_changed, this, &ChannelNode::self_changed);
}

QVariant ChannelNode::data() {
    return channel_->name();
}

void ChannelNode::self_changed(bool joined) {
    if (joined) {
        MemberNode *mn = new MemberNode(this);
        children.append(mn);
    } else {
        // TODO, handle leaving
    }
}

MemberNode::MemberNode(ChannelNode *parent) : Node(NodeType::MemberNode, parent),
    fr_(nullptr) {
}

QVariant MemberNode::data() {
    if (fr_) {
        return "friend";
    } else {
        return "self";
    }
}
