#include <gstreamermm.h>
#include <iostream>

#include "audiocall.hpp"

template <typename T>
using ref = Glib::RefPtr<T>;

namespace {
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
}

AudioCall::AudioCall(QObject *parent) : QObject(parent), outputstream(nullptr)
{

}

void AudioCall::create_instance() {
    outputstream = static_cast<ToxOutputStream*> (g_object_new(TOX_TYPE_OUTPUT, nullptr));
    reference = Glib::wrap(reinterpret_cast<GOutputStream*>(outputstream));
}

void AudioCall::create_pipeline() {
    ref<Glib::MainLoop> mainloop = Glib::MainLoop::create();
    ref<Gst::Pipeline> pipeline = Gst::Pipeline::create("gst-test");

    auto src = make_element("audiotestsrc");
    auto sink = make_element("giostreamsink");

    if(!pipeline || !src || !sink) {
        std::cerr << "One element could not be created\n";
        return;
    }

    sink->set_property("stream", reference);
}
