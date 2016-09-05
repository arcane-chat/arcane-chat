#ifndef PULSE_STREAM_H
#define PULSE_STREAM_H

#include <QObject>

typedef struct pa_context pa_context;
typedef struct pa_stream pa_stream;

namespace pulse {

class PAThreadedMainLoop;

class Stream : public QObject
{
    Q_OBJECT
public:
    explicit Stream(PAThreadedMainLoop *parent = 0);
    ~Stream();

    /*! write some audio to the playback stream
     * @param samples an array of signed 16bit samples
     * @param count number of samples in the array
     */
    void write(int16_t *samples, int count);
signals:

public slots:
private:
    pa_stream *stream;
    PAThreadedMainLoop *loop;
};

} // namespace pulse

#endif // PULSE_STREAM_H
