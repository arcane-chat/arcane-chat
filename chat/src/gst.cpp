#include <gstreamermm.h>
#include <glibmm.h>
#include <giomm/init.h>
#include <giomm/inputstream.h>
#include <giomm/outputstream.h>
#include <iostream>
#include <iomanip>
#include <sys/socket.h>
#include <thread>
#include "toxoutputstream.hpp"
#include "toxinputstream.hpp"
#include <QThread>

template <typename T>
using ref = Glib::RefPtr<T>;

class GstException : public std::exception {
private:
    std::string description_;

public:
    explicit GstException(std::string desc) : description_(desc) {}

    virtual const char* what() const throw() {
        return description_.c_str();
    }
};

ref<Gst::Element> make_element(const std::string& name) {
    ref<Gst::Element> result = Gst::ElementFactory::create_element(name);
    if(!result) { throw GstException(name + " could not be created."); }
    return result;
}

void test_toxoutputstream(ref<Gio::OutputStream> gos) {
    ref<Glib::MainLoop> mainloop = Glib::MainLoop::create();
    ref<Gst::Pipeline> pipeline = Gst::Pipeline::create("gst-test");

    auto src = make_element("audiotestsrc");
    auto sink = make_element("giostreamsink");

    if(!pipeline || !src || !sink) {
        std::cerr << "One element could not be created\n";
        return;
    }

    sink->set_property("stream", gos);

    try {
        pipeline->add(src)->add(sink);
    } catch(const Glib::Error& ex) {
        std::cerr << "Error while adding elements to the pipeline: "
                  << ex.what() << "\n";
        return;
    }

    try {
        src->link(sink);
    } catch(const std::runtime_error& ex) {
        std::cout << "Exception while linking elements: " << ex.what() << "\n";
    }

    pipeline->set_state(Gst::STATE_PLAYING);
    mainloop->run();
    pipeline->set_state(Gst::STATE_NULL);
}

void test_toxinputstream(ref<Gio::InputStream> gis) {
    ref<Glib::MainLoop> mainloop = Glib::MainLoop::create();
    ref<Gst::Pipeline> pipeline = Gst::Pipeline::create("gst-test");

    auto src = make_element("giostreamsrc");
    auto sink = make_element("pulsesink");

    if(!pipeline || !src || !sink) {
        std::cerr << "One element could not be created\n";
        return;
    }

    sink->set_property("stream", gis);

    try {
        pipeline->add(src)->add(sink);
    } catch(const Glib::Error& ex) {
        std::cerr << "Error while adding elements to the pipeline: "
                  << ex.what() << "\n";
        return;
    }

    try {
        src->link(sink);
    } catch(const std::runtime_error& ex) {
        std::cout << "Exception while linking elements: " << ex.what() << "\n";
    }

    pipeline->set_state(Gst::STATE_PLAYING);
    mainloop->run();
    pipeline->set_state(Gst::STATE_NULL);
}

int main(int argc, char** argv) {
    Gst::init(argc, argv);
    Gio::init();

    gpointer out = g_object_new(TOX_TYPE_OUTPUT, nullptr);
    qDebug() << "instance" << out << QThread::currentThread();
    ref<Gio::OutputStream> gos = Glib::wrap(static_cast<GOutputStream*>(out));
    test_toxoutputstream(gos);

    // gpointer in = g_object_new(TOX_TYPE_INPUT, nullptr);
    // qDebug() << "instance" << in << QThread::currentThread();
    // ref<Gio::InputStream> gis = Glib::wrap(static_cast<GInputStream*>(in));
    // test_toxinputstream(gis);
}

// #include <iostream>
// #include "toxoutputstream.hpp"
// #include <QCoreApplication>
// #include <Qt5GStreamer/QGlib/Error>
// #include <Qt5GStreamer/QGlib/Connect>
// #include <Qt5GStreamer/QGst/Init>
// #include <Qt5GStreamer/QGst/Bus>
// #include <Qt5GStreamer/QGst/Pipeline>
// #include <Qt5GStreamer/QGst/Parse>
// #include <Qt5GStreamer/QGst/Message>
// #include <Qt5GStreamer/QGst/Utils/ApplicationSink>
// #include <Qt5GStreamer/QGst/Utils/ApplicationSource>
//
// const char* caps = "audio/x-opus, channel-mapping-family=(int)0";
// // const char* caps = "video/x-vp8";
//
// QString pipe1 = QString("audiotestsrc ! opusenc"
//                         " ! appsink name=\"a\" caps=\"%2\"").arg(caps);
// // QString pipe1 = QString("v4l2src ! vp8enc deadline=20000 threads=8"
// //                         " ! appsink name=\"a\" caps=\"%2\"").arg(caps);
//
// QString pipe2 = QString("appsrc name=\"b\" is-live=true caps=\"%2\" format=3"
//                         " ! decodebin ! pulsesink").arg(caps);
// // QString pipe2 = QString("appsrc name=\"b\" is-live=true caps=\"%2\" format=3"
// //                         " ! decodebin ! autovideosink").arg(caps);
