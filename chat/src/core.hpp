#include <tox/tox.h>

#include <QObject>
#include <QTimer>

namespace chat {
    class Core : public QObject {
    Q_OBJECT
    public:
        Core(Tox *tox);
    private slots:
        void checkTox();
    private:
        Tox* tox;
        QTimer iterator;
    };
}
