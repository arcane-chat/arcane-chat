#pragma once

#include <QAbstractItemModel>
#include <QList>

#include "friend.hpp"

enum class NodeType { Root, LegacyFolder, LegacyFriend };

class Node : public QObject {
    Q_OBJECT
public:
    Node(NodeType type, Node* parent) : type(type), parent(parent) {}
    virtual QVariant data();

    NodeType type;
    Node* parent;
    QList<Node*> children;

signals:
    void changed(Node* self);
};

class FriendNode : public Node {
    Q_OBJECT
public:
    FriendNode(Node* parent, chat::Friend* f);
    virtual QVariant data();

    chat::Friend* f;
private slots:
    void connection_changed(tox::LinkType old_state, tox::LinkType new_state);
    void message(bool action, QByteArray message);
};

class ChannelModel : public QAbstractItemModel {
    Q_OBJECT
public:
    explicit ChannelModel(QMap<uint32_t, chat::Friend*> friends);
    QModelIndex index(int row, int column, const QModelIndex& parent) const;
    QModelIndex parent(const QModelIndex& child) const;
    int rowCount(const QModelIndex& parent) const;
    int columnCount(const QModelIndex& parent) const;
    QVariant data(const QModelIndex& index, int role) const;
    Node* getNode(const QModelIndex& index) const;

    Node* root;

private:
private slots:
    void node_changed(Node*);
};
