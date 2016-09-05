#pragma once

#include <cstdint>
#include <pulse/proplist.h>
#include <pulse/thread-mainloop.h>
#include <pulse/context.h>
#include <pulse/stream.h>
#include <QObject>

namespace pulse {
    class Stream;
    class PAThreadedMainLoop : public QObject {
        friend class Stream;
        Q_OBJECT
    public:
        PAThreadedMainLoop();
        ~PAThreadedMainLoop();
        void state_change_callback();

        Stream* playback;

    protected:
        pa_context* context;
        pa_threaded_mainloop* loop;

    private:
        static int set_prop(pa_proplist* p, const char* key, QString value);
        void open_stream();
    };
}
