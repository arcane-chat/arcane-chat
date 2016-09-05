#include <gstreamermm.h>
#include <glibmm.h>
#include <giomm/socket.h>
#include <iostream>
#include <iomanip>
#include <sys/socket.h>
#include <thread>

template <typename T>
using ref = Glib::RefPtr<T>;

void sender(int fd) {
    ref<Glib::MainLoop> mainloop = Glib::MainLoop::create();
    ref<Gst::Pipeline> pipeline = Gst::Pipeline::create("gst-test");

    ref<Gst::Element> src = Gst::ElementFactory::create_element("audiotestsrc");
    if(!src) {
        std::cerr << "audiotestsrc could not be created.\n";
    }
    ref<Gst::Element> sink = Gst::ElementFactory::create_element("fdsink");
    if(!sink) {
        std::cerr << "fdsink could not be created.\n";
    }

    if(!pipeline || !src || !sink) {
        std::cerr << "One element could not be created\n";
        return;
    }

    sink->set_property("fd", fd);

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

void receiver(int fd) {
    ref<Glib::MainLoop> mainloop = Glib::MainLoop::create();
    ref<Gst::Pipeline> pipeline = Gst::Pipeline::create("gst-test");

    ref<Gst::Element> src = Gst::ElementFactory::create_element("shmsrc");
    if(!src) {
        std::cerr << "shmsrc could not be created.\n";
    }
    ref<Gst::Element> sink = Gst::ElementFactory::create_element("pulsesink");
    if(!sink) {
        std::cerr << "pulsesink could not be created.\n";
    }

    if(!pipeline || !src || !sink) {
        std::cerr << "One element could not be created\n";
        return;
    }

    src->set_property("fd", fd);

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
    int vec[2];
    socketpair(AF_LOCAL, SOCK_DGRAM, 0, vec);
    std::thread sender_thread{sender, vec[0]};
    std::thread receiver_thread{receiver, vec[1]};
    sender_thread.join();
    receiver_thread.join();
}
