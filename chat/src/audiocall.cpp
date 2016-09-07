#include <gstreamermm.h>
#include <iostream>

#include "audiocall.hpp"
#include "core.hpp"

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
    outputstream->call = this;
    reference = Glib::wrap(reinterpret_cast<GOutputStream*>(outputstream));
}

void AudioCall::create_pipeline() {
    core->call_start(fr);
    mainloop = Glib::MainLoop::create();
    ref<Gst::Pipeline> pipeline = Gst::Pipeline::create("gst-test");

    auto src = make_element("audiotestsrc");
    auto sink = make_element("giostreamsink");

    if(!pipeline || !src || !sink) {
        std::cerr << "One element could not be created\n";
        return;
    }

    sink->set_property("stream", reference);

    try {
        pipeline->add(src)->add(sink);
    } catch(const Glib::Error& ex) {
        std::cerr << "Error while adding elements to the pipeline: " << ex.what() << "\n";
        return;
    }

    pipeline->set_state(Gst::STATE_PLAYING);
    mainloop->run();
    pipeline->set_state(Gst::STATE_NULL);
}

void AudioCall::stop_everything()
{
    mainloop->quit();
    core->call_stop(fr);
}

ssize_t AudioCall::write_fn(QByteArray data) {
    core->call_data(fr,data);
}
