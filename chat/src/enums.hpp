#pragma once

namespace tox {
    //! FIXME: doc
    enum class ProxyType {
        none,  //!< FIXME: doc
        http,  //!< FIXME: doc
        socks5 //!< FIXME: doc
    };

    //! FIXME: doc
    enum class SaveDataType {
        none,      //!< FIXME: doc
        tox_save,  //!< FIXME: doc
        secret_key //!< FIXME: doc
    };

    //! Message types for handle_message.
    enum class MessageType {
        normal, //!< Normal text message, like /msg on IRC.
        action  //!< A message describing an user action, like /me from IRC.
    };

    //! FIXME: doc
    enum class LinkType {
        none, //!< FIXME: doc
        tcp,  //!< FIXME: doc
        udp   //!< FIXME: doc
    };
} // namespace tox
