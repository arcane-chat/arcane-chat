#pragma once

#include <cstdint>
#include <pulse/proplist.h>
#include <pulse/thread-mainloop.h>
#include <pulse/context.h>
#include <pulse/stream.h>
#include <QObject>

class PAThreadedMainLoop : public QObject {
Q_OBJECT
public:
    PAThreadedMainLoop();
    ~PAThreadedMainLoop();
    void write(int16_t *samples, int count);
    void state_change_callback();
private:
    static int set_prop(pa_proplist *p, const char *key, QString value);
    void open_stream();

    pa_threaded_mainloop *loop;
    pa_context *context;
    pa_stream *stream;
};

PAThreadedMainLoop *pa_test_init();
void pa_test_write(PAThreadedMainLoop *loop, int16_t *samples, int count);
void pa_test_stop(PAThreadedMainLoop *loop);
