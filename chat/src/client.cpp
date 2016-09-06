#include <iostream>
#include <boost/algorithm/hex.hpp>
#include <vector>
#include <tox/tox.h>
#include <cassert>
#include <cstdio>
#include <cstring>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>

#include <QApplication>

#include "options.hpp"
#include "core.hpp"
#include "utils.hpp"
#include "tracer.hpp"
#include "friend.hpp"
#include "mainwindow.h"

namespace tox::bootstrap {
    constexpr const char* address = "23.226.230.47";

    constexpr int port = 33445;

    constexpr const char* key =
        "A09162D68618E742FFBCA1C2C70385E6679604B2D80EA6E84AD0996A1AC8A074";
} // namespace tox::bootstrap

std::vector<uint8_t> from_hex(const std::string& hex) {
    std::vector<uint8_t> out;
    out.resize(hex.size() / 2);
    boost::algorithm::unhex(hex.begin(), hex.end(), out.begin());
    return out;
}

void saveState(Tox* tox) {
    size_t size = tox_get_savedata_size(tox);
    uint8_t* savedata = new uint8_t[size];
    tox_get_savedata(tox, savedata);
    int fd = open("/tmp/savedata", O_TRUNC | O_WRONLY | O_CREAT, 0644);
    assert(fd);
    ssize_t written = write(fd, savedata, size);
    assert(written > 0);
    close(fd);
}

void connection_status(Tox* tox,
                       TOX_CONNECTION connection_status,
                       void* user_data) {
    uint8_t toxid[TOX_ADDRESS_SIZE];
    tox_self_get_address(tox, toxid);
    std::string tox_printable_id = to_hex(toxid, TOX_ADDRESS_SIZE);

    const char* msg = nullptr;

    switch(connection_status) {
    case TOX_CONNECTION_NONE:
        msg = "offline";
        std::cout << "connection lost\n";
        break;
    case TOX_CONNECTION_TCP:
        msg = "connected via tcp";
        std::cout << "tcp connection established\n";
        break;
    case TOX_CONNECTION_UDP:
        msg = "connected via udp";
        std::cout << "udp connection established\n";
        break;
    }

    if(msg != nullptr) {
        std::cout << "status = " << msg << ", "
                  << "id = " << tox_printable_id << "\n";
    }

    saveState(tox);
    fflush(stdout);
}

void MyFriendRequestCallback(Tox* tox,
                             const uint8_t* public_key,
                             const uint8_t* message,
                             size_t length,
                             void* user_data) {
    std::cout << __func__ << "\n";
    TOX_ERR_FRIEND_ADD error;
    tox_friend_add_norequest(tox, public_key, &error);
    switch(error) {
    case TOX_ERR_FRIEND_ADD_OK: break;
    case TOX_ERR_FRIEND_ADD_ALREADY_SENT:
        fputs("already sent\n", stderr);
        break;
    case TOX_ERR_FRIEND_ADD_BAD_CHECKSUM: fputs("crc error\n", stderr); break;
    default: std::cerr << "error code: " << error << "\n";
    }
    saveState(tox);
}

Tox* initTox() {
    Tox* tox;
    struct Tox_Options* opts = tox_options_new(nullptr);
    opts->start_port = 33445;
    opts->end_port = 33445 + 100;
    int oldstate = open("/tmp/savedata", O_RDONLY);
    if(oldstate >= 0) {
        struct stat info;
        fstat(oldstate, &info);
        uint8_t* temp = new uint8_t[info.st_size];
        ssize_t size = read(oldstate, temp, info.st_size);
        close(oldstate);
        assert(size == info.st_size);
        opts->savedata_type = TOX_SAVEDATA_TYPE_TOX_SAVE;
        opts->savedata_data = temp;
        opts->savedata_length = size;
    }
    TOX_ERR_NEW new_error;
    tox = tox_new(opts, &new_error);
    if(!tox) {
        opts->ipv6_enabled = false;
        tox = tox_new(opts, &new_error);
    }
    if(opts->savedata_data) {
        delete opts->savedata_data;
    }
    tox_options_free(opts);
    opts = 0;
    tox_callback_self_connection_status(tox, &connection_status, nullptr);
    tox_callback_friend_request(tox, MyFriendRequestCallback, nullptr);
    return tox;
}

void syncTox(Tox* tox) { saveState(tox); }

void closeTox(Tox* tox) {
    saveState(tox);
    tox_kill(tox);
}

bool keep_running = true;

void handler(int signum) {
    std::cout << "Quitting...\n";
    keep_running = false;
    QCoreApplication::quit();
}

int opus_main();

int main(int argc, char** argv) {
    return opus_main();

    QApplication app(argc, argv);
    struct sigaction interrupt;
    memset(&interrupt, 0, sizeof(interrupt));
    interrupt.sa_handler = &handler;
    sigaction(SIGINT, &interrupt, nullptr);
    sigaction(SIGTERM, &interrupt, nullptr);

    Tox* tox = initTox();
    uint8_t toxid[TOX_ADDRESS_SIZE];
    tox_self_get_address(tox, toxid);
    std::cout << "my id is " << to_hex(toxid, TOX_ADDRESS_SIZE) << "\n";
    std::vector<uint8_t> bootstrap_pub_key = from_hex(bootstrap::key);
    tox_bootstrap(tox, bootstrap::address, bootstrap::port,
                  bootstrap_pub_key.data(), nullptr);

    chat::Core core(tox);
    Tracer* tracer = new Tracer(&core);
    QList<chat::Friend*> friends = core.getFriends();
    MainWindow* mw = new MainWindow(friends, &core);
    mw->show();

    int ret = app.exec();
    closeTox(tox);
    std::cout << "clean shutdown\n";
    return ret;
}
