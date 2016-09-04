#include <tox/tox.h>

#include <QObject>
#include <QTimer>

namespace chat {
    class Core : public QObject {
    Q_OBJECT
    public:
        Core(Tox *tox);
        void handleMessage(uint32_t friend_number, TOX_MESSAGE_TYPE type,
                         const unsigned char *message, size_t length);
    signals:
        void onMessage(uint32_t friend_number, TOX_MESSAGE_TYPE type, QString message);
    private slots:
        void checkTox();
    private:
        Tox* tox;
        QTimer iterator;
    };
}
