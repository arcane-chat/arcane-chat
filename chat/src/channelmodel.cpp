#include <QDebug>
#include <cassert>

#include "channelmodel.hpp"

ChannelModel::ChannelModel(QList<chat::Friend*> friends) {
    root = new Node(NodeType::Root, nullptr);
    Node* legacyFolder = new Node(NodeType::LegacyFolder, root);
    root->children.append(legacyFolder);

    for(chat::Friend* f : friends) {
        FriendNode* t = new FriendNode(legacyFolder, f);
        connect(t,    SIGNAL(changed(Node*)),
                this, SLOT(node_changed(Node*)));
        legacyFolder->children.append(t);
    }
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
    if(type == NodeType::LegacyFolder) {
        return "Legacy Clients";
    } else {
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

    return QString("%1 - %2 - %3").arg(state).arg(f->name).arg(f->last_message);
}

FriendNode::FriendNode(Node* parent, chat::Friend* f)
    : Node(NodeType::LegacyFriend, parent), f(f) {
    connect(f,    SIGNAL(connection_changed(tox::LinkType, tox::LinkType)),
            this, SLOT(connection_changed(tox::LinkType, tox::LinkType)));
    connect(f,    SIGNAL(message(bool, QByteArray)),
            this, SLOT(message(bool, QByteArray)));
}

void FriendNode::connection_changed(tox::LinkType old_state,
                                    tox::LinkType new_state) {
    emit changed(this);
}

void FriendNode::message(bool action, QByteArray message) {
    emit changed(this);
}

void ChannelModel::node_changed(Node* node) {
    assert(node->parent);
    int row = node->parent->children.indexOf(node);
    QModelIndex index = createIndex(row, 0, node);
    emit dataChanged(index, index);
}
