#pragma once

namespace tox {
    enum class proxy_type {
        none = TOX_PROXY_TYPE_NONE,
        http = TOX_PROXY_TYPE_HTTP,
        socks5 = TOX_PROXY_TYPE_SOCKS5
    };

    enum class savedata_type {
        none = TOX_SAVEDATA_TYPE_NONE,
        tox_save = TOX_SAVEDATA_TYPE_TOX_SAVE,
        secret_key = TOX_SAVEDATA_TYPE_SECRET_KEY
    };

} // namespace tox
