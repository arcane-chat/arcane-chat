#include <iostream>
#include <sstream>
#include <tox/tox.h>
#include <cassert>
#include <cstdio>
#include <cstring>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>

#define BOOTSTRAP_ADDRESS "23.226.230.47"
#define BOOTSTRAP_PORT 33445
#define BOOTSTRAP_KEY "A09162D68618E742FFBCA1C2C70385E6679604B2D80EA6E84AD0996A1AC8A074"

void to_hex(char *a, const uint8_t *p, size_t size) {
    char buffer[3];
    for(size_t i = 0; i < size; i++) {
        snprintf(buffer, 3, "%02x", p[i]);
        a[i*2]     = buffer[0];
        a[i*2 + 1] = buffer[1];
    }
}

void hex_string_to_bin(const char* hex_string, uint8_t* ret) {
    size_t len = strlen(hex_string) / 2;
    const char* pos = hex_string;
    for(size_t i = 0; i < len; ++i, pos += 2) {
        sscanf(pos, "%2hhx", &ret[i]);
    }
}

void saveState(Tox* tox) {
    size_t size = tox_get_savedata_size(tox);
    uint8_t* savedata = new uint8_t[size];
    tox_get_savedata(tox, savedata);
    int fd = open("/tmp/savedata", O_TRUNC|O_WRONLY|O_CREAT, 0644);
    assert(fd);
    ssize_t written = write(fd, savedata, size);
    assert(written > 0);
    close(fd);
}

void connection_status(Tox*           tox,
                       TOX_CONNECTION connection_status,
                       void*          user_data) {
    uint8_t toxid[TOX_ADDRESS_SIZE];
    tox_self_get_address(tox,toxid);
    char tox_printable_id[TOX_ADDRESS_SIZE * 2 + 1];
    memset(tox_printable_id, 0, sizeof(tox_printable_id));
    to_hex(tox_printable_id, toxid,TOX_ADDRESS_SIZE);

    char buffer[128];
    const char *msg = 0;

    switch (connection_status) {
    case TOX_CONNECTION_NONE:
        msg = "offline";
        puts("connection lost");
        break;
    case TOX_CONNECTION_TCP:
        msg = "connected via tcp";
        puts("tcp connection established");
        break;
    case TOX_CONNECTION_UDP:
        msg = "connected via udp";
        puts("udp connection established");
        break;
    }
    if(msg) { snprintf(buffer,120,"STATUS=%s, id=%s",msg,tox_printable_id); }
    saveState(tox);
    fflush(stdout);
}

void FriendConnectionUpdate(Tox *tox, uint32_t friend_number,
                            TOX_CONNECTION connection_status,
                            void *user_data) {
    puts(__func__);
}

void MyFriendRequestCallback(Tox *tox, const uint8_t *public_key,
                             const uint8_t *message, size_t length,
                             void *user_data) {
    puts(__func__);
    TOX_ERR_FRIEND_ADD error;
    tox_friend_add_norequest(tox, public_key, &error);
    switch (error) {
    case TOX_ERR_FRIEND_ADD_OK:
        break;
    case TOX_ERR_FRIEND_ADD_ALREADY_SENT:
        fputs("already sent\n", stderr);
        break;
    case TOX_ERR_FRIEND_ADD_BAD_CHECKSUM:
        fputs("crc error\n", stderr);
        break;
    default:
        fprintf(stderr, "err code %d\n", error);
    }
    saveState(tox);
}

void MyFriendMessageCallback(Tox *tox, uint32_t friend_number,
                             TOX_MESSAGE_TYPE type, const uint8_t *message,
                             size_t length, void *user_data) {
    std::cout << "message: \"" << message << "\"\n";
}

void MyFriendLossyPacket(Tox *tox, uint32_t friend_number,
                         const uint8_t *data, size_t length,
                         void *user_data) {
    char str[2 * length + 1];
    to_hex(str, data, length);
    std::cout << "data: " << str << "\n";
}

void MyFriendLosslessPacket(Tox *tox, uint32_t friend_number,
                            const uint8_t *data, size_t length,
                            void *user_data) {
    char str[2 * length + 1];
    to_hex(str, data, length);
    std::cout << "data: " << str << "\n";
}

Tox* initTox() {
    Tox* tox;
    struct Tox_Options* opts = tox_options_new(NULL);
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
    if(opts->savedata_data) { delete opts->savedata_data; }
    tox_options_free(opts);
    opts = 0;
    tox_callback_self_connection_status(tox, &connection_status, 0);
    tox_callback_friend_request(tox, MyFriendRequestCallback, NULL);
    tox_callback_friend_message(tox, MyFriendMessageCallback, NULL);
    tox_callback_friend_connection_status(tox, FriendConnectionUpdate, NULL);
    tox_callback_friend_lossy_packet(tox, MyFriendLossyPacket, NULL);
    tox_callback_friend_lossless_packet(tox, MyFriendLosslessPacket, NULL);
    std::string username = ({
            std::stringstream ss;
            ss << "fuspr-" << rand();
            ss.str();
        });
    tox_self_set_name(tox, reinterpret_cast<const uint8_t*>(username.c_str()),
                      username.size(), NULL);
    return tox;
}

void syncTox(Tox* tox) {
    saveState(tox);
}

void closeTox(Tox* tox) {
    saveState(tox);
    tox_kill(tox);
}

bool keep_running = true;

void handler(int signum) {
    std::cout << "Quitting...\n";
    keep_running = false;
}

int main(int argc, char** argv) {
    struct sigaction interrupt;
    memset(&interrupt,0,sizeof(interrupt));
    interrupt.sa_handler = &handler;
    sigaction(SIGINT, &interrupt, NULL);

    Tox* tox = initTox();
    uint8_t toxid[TOX_ADDRESS_SIZE];
    tox_self_get_address(tox, toxid);
    char tox_printable_id[TOX_ADDRESS_SIZE * 2 + 1];
    memset(tox_printable_id, 0, sizeof(tox_printable_id));
    to_hex(tox_printable_id, toxid, TOX_ADDRESS_SIZE);
    printf("my id is %s\n", tox_printable_id);
    uint8_t *bootstrap_pub_key = new uint8_t[TOX_PUBLIC_KEY_SIZE];
    hex_string_to_bin(BOOTSTRAP_KEY, bootstrap_pub_key);
    tox_bootstrap(tox, BOOTSTRAP_ADDRESS, BOOTSTRAP_PORT,
                  bootstrap_pub_key, NULL);
    while(keep_running) {
        int interval = tox_iteration_interval(tox);
        usleep(1000 * interval);
        tox_iterate(tox);
        // ^^^ will call the callback functions defined and registered
    }
    closeTox(tox);
    std::cout << "Hello, World!\n";
}
