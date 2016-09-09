#undef QT_NO_DEBUG

#include <QCoreApplication>
#include <QFile>
#include <QDebug>
#include <QDataStream>
#include <QHostAddress>

int main(int argc, char **argv) {
    QCoreApplication app(argc, argv);

    QFile fh(app.arguments().at(1));
    fh.open(QFile::ReadOnly);
    QByteArray data = fh.readAll();
    qDebug() << data.mid(0,20).toHex();
    QDataStream ds(data);
    ds.setByteOrder(QDataStream::LittleEndian);
    uint32_t a,magic1;
    ds >> a >> magic1;
    Q_ASSERT(a == 0);
    Q_ASSERT(magic1 == 0x15ED1B1F);

    bool stop = false;

    while (!stop) {
        uint32_t section_size;
        uint16_t type;
        uint16_t magic2;

        ds >> section_size >> type >> magic2;
        Q_ASSERT(magic2 == 0x01ce);

        char buffer[section_size];
        ds.readRawData(buffer,section_size);

        QByteArray section(buffer,section_size);
        QDataStream ds2(section);
        ds2.setByteOrder(QDataStream::LittleEndian);
        switch (type) {
        case 1:
            qDebug() << "toxid" << section_size;
            char nospam[4];
            char pub[32];
            char priv[32];
            ds2.readRawData(nospam,4);
            ds2.readRawData(pub,32);
            ds2.readRawData(priv,32);
            qDebug() << QByteArray(pub,32).toHex() << QByteArray(nospam,4).toHex() << "crc";// no private for you! << QByteArray(priv,32).toHex();
            break;
        case 2:
            qDebug() << "dht" << section_size;
            uint32_t magic3;
            ds2 >> magic3;
            Q_ASSERT(magic3 == 0x159000d);
            while (!ds2.atEnd()) {
                uint32_t dht_section_size;
                uint16_t dht_type;
                uint16_t magic4;
                ds2 >> dht_section_size >> dht_type >> magic4;
                qDebug() << dht_section_size << dht_type;
                Q_ASSERT(magic4 == 0x11ce);
                Q_ASSERT(dht_type == 4); // nodelist
                char buffer2[dht_section_size];
                ds2.readRawData(buffer2, dht_section_size);
                QByteArray nodelist(buffer2, dht_section_size);
                QDataStream ds3(nodelist);
                ds3.setByteOrder(QDataStream::BigEndian);
                while (!ds3.atEnd()) {
                    uint8_t proto;
                    union {
                        char raw[16];
                        quint8 v6[16];
                        quint32 v4;
                    } address;
                    //char address[16];
                    uint16_t port;
                    char nodeid[32];
                    ds3 >> proto;
                    if (proto == 2) {
                        //ds3.readRawData(address.raw,4);
                        ds3 >> address.v4;
                    } else if (proto == 10) ds3.readRawData(address.raw,16);
                    else Q_ASSERT(false);
                    ds3 >> port;
                    ds3.readRawData(nodeid,32);
                    QByteArray nodeidhex(nodeid,32);
                    QHostAddress ip;
                    if (proto == 2) ip = QHostAddress(address.v4);
                    else ip = QHostAddress(address.v6);

                    qDebug() << ip << port << nodeidhex.toHex();
                }
            }
            break;
        case 3:
            qDebug() << "friends" << section_size;
            break;
        case 4:
            qDebug() << "name" << section_size;
            break;
        case 5:
            qDebug() << "status msg" << section_size;
            break;
        case 6:
            qDebug() << "status" << section_size;
            break;
        case 0xa:
            qDebug() << "tcp relays" << section_size;
            break;
        case 0xb:
            qDebug() << "path nodes" << section_size;
            break;
        case 0xff:
            qDebug() << "eof" << section_size;
            stop = true;
        }
    }

    return 0;
}
