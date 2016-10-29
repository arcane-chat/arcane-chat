#include <zmq.hpp>
#include <string>
#include <sstream>
#include <vector>
#include <QThread>

//! FIXME: doc
template <typename T>
class zmq_channel {
private:
    std::string name_;

public:
    zmq_channel(const std::string &name) : name_(name) {}
    zmq_channel(const zmq_channel &) = delete;

    zmq_channel& operator=(const zmq_channel &) = delete;

    const std::string &get_name() const { return name_; }
};

//! FIXME: doc
template <typename T>
class ro_channel : public zmq_channel<T> {
private:
    zmq::socket_t sub_socket_;

    bool recv_generic(T& value, int flags) {
        zmq::message_t msg;
        if(sub_socket_.recv(&msg, flags)) { return false; }

        {
            std::string name { (static_cast<char*>(msg.data())), msg.size() };
            if(name != zmq_channel<T>::get_name()) { return false; }
        }

        if(!msg.more()) { return false; }

        if(sub_socket_.recv(&msg, flags)) { return false; }

        std::string encoded { (static_cast<char*>(msg.data())), msg.size() };

        return value.ParseFromString(encoded);
    }

public:
    ro_channel(std::string name, zmq::context_t& ctx)
        : zmq_channel<T>(name), sub_socket_(ctx, ZMQ_SUB) {
        sub_socket_.connect("tcp://localhost:5560");
        sub_socket_.setsockopt(ZMQ_SUBSCRIBE, name.c_str(), name.size());
    }

    bool recv(T& out) {
        return recv_generic(out, 0);
    }

    bool recv_async(T& out) {
        return recv_generic(out, ZMQ_NOBLOCK);
    }
};

//! FIXME: doc
template <typename T>
class wo_channel : public zmq_channel<T> {
private:
    zmq::socket_t pub_socket_;

    bool send_generic(T value, int flags) {
        std::string name = zmq_channel<T>::get_name();
        if(pub_socket_.send(name.begin(), name.end(), flags | ZMQ_SNDMORE)) {
            return false;
        }
        std::string message;
        if(!value.SerializeToString(message)) { return false; }
        return !pub_socket_.send(message.begin(), message.end(), flags);
    }

public:
    wo_channel(std::string name, zmq::context_t& ctx)
        : zmq_channel<T>(name), pub_socket_(ctx, ZMQ_PUB) {
        pub_socket_.connect("tcp://localhost:5559");
    }

    bool send(T value) {
        return send_generic(value, 0);
    }

    bool send_async(T value) {
        return send_generic(value, ZMQ_NOBLOCK);
    }
};

//! FIXME: doc
template <typename T>
class rw_channel : public ro_channel<T>, public wo_channel<T> {
public:
    rw_channel(std::string name, zmq::context_t& ctx)
        : ro_channel<T>(name, ctx), wo_channel<T>(name, ctx) {}
};

//! FIXME: doc
class task : public QThread {
    Q_OBJECT
private:
    std::vector<std::string> arguments_;
    zmq::context_t* ctx;
public:
    task(std::vector<std::string> arguments) : arguments_(arguments) {}
    void launch() {}
    void run() Q_DECL_OVERRIDE {
        launch();
    }
};
