#include <gstreamermm.h>
#include <glibmm.h>
#include <iostream>
#include <iomanip>

template <typename T>
using ref = Glib::RefPtr<T>;

namespace {
    ref<Glib::MainLoop> mainloop;
    ref<Gst::Pipeline> pipeline;
    ref<Gst::Element> decoder;
    gulong data_probe_id = 0;

    bool on_timeout() {
        Gst::Format fmt = Gst::FORMAT_TIME;
        gint64 pos = 0;
        gint64 len = 0;

        ref<Gst::Query> query = Gst::QueryPosition::create(fmt);

        if(pipeline->query(query) && pipeline->query_duration(fmt, len)) {
            if(auto query_pos = ref<Gst::QueryPosition>::cast_static(query)) {
                pos = query_pos->parse();
            }

            auto print_time = [](gint64 time) {
                std::cout << std::right << std::setfill('0') << std::setw(3);
                std::cout << Gst::get_hours(time);
                std::cout << ":" << std::setw(2);
                std::cout << Gst::get_minutes(time);
                std::cout << ":" << std::setw(2);
                std::cout << Gst::get_seconds(time);
                std::cout << "." << std::setw(9) << std::left;
                std::cout << Gst::get_fractional_seconds(time);
            };

            std::cout << std::right << "Time: " << std::setfill('0');
            print_time(pos);
            std::cout << std::right << "/";
            print_time(len);
            std::cout << std::endl;
        }

        return true;
    }

    bool on_bus_message(const ref<Gst::Bus>& /* bus */,
                        const ref<Gst::Message>& message) {
        switch(message->get_message_type()) {
        case Gst::MESSAGE_EOS:
            std::cout << std::endl << "End of stream" << std::endl;
            mainloop->quit();
            return false;
        case Gst::MESSAGE_ERROR: {
            auto msgError = ref<Gst::MessageError>::cast_static(message);
            if(msgError) {
                std::cerr << "Error: " << (msgError->parse().what()) << "\n";
            } else {
                std::cerr << "Error.\n";
            }
            mainloop->quit();
            return false;
        }
        default: { break; }
        }

        return true;
    }

    void on_parser_pad_added(const ref<Gst::Pad>& newPad) {
        // We can now link this pad with the audio decoder
        std::cout << "Dynamic pad created. Linking parser/decoder.\n";
        ref<Gst::Pad> sinkPad = decoder->get_static_pad("sink");
        Gst::PadLinkReturn ret = newPad->link(sinkPad);

        if((ret != Gst::PAD_LINK_OK) && (ret != Gst::PAD_LINK_WAS_LINKED)) {
            std::cerr << "Linking of pads " << (newPad->get_name()) << " and "
                      << (sinkPad->get_name()) << " failed.\n";
        }
    }

    Gst::PadProbeReturn on_sink_pad_have_data(const ref<Gst::Pad>& pad,
                                              const Gst::PadProbeInfo&) {
        std::cout << "Sink pad '" << (pad->get_name()) << "' has received data;"
                  << " will now remove sink data probe id: " << data_probe_id
                  << "\n";
        pad->remove_probe(data_probe_id);
        return Gst::PAD_PROBE_OK;
    }

} // anonymous namespace

int main(int argc, char** argv) {
    Gst::init(argc, argv);

    if(argc < 2) {
        std::cout << "Usage: " << argv[0] << " <ogg/vorbis filename>\n";
        return 1;
    }

    const std::string filename = argv[1];

    mainloop = Glib::MainLoop::create();

    pipeline = Gst::Pipeline::create("audio-player");

#define CREATE_ELEMENT(var, name)                                              \
    var = Gst::ElementFactory::create_element(name);                           \
    if(!var) {                                                                 \
        std::cerr << name << " element could not be created.\n";               \
    }

    ref<Gst::Element> source, parser, conv, sink;

    CREATE_ELEMENT(source, "filesrc");
    CREATE_ELEMENT(parser, "oggdemux");
    CREATE_ELEMENT(decoder, "vorbisdec");
    CREATE_ELEMENT(conv, "audioconvert");
    CREATE_ELEMENT(sink, "pulsesink");

#undef CREATE_ELEMENT

    if(!pipeline || !source || !parser || !decoder || !conv || !sink) {
        std::cerr << "One element could not be created\n";
        return 1;
    }

    ref<Gst::Pad> pad = sink->get_static_pad("sink");
    if(pad) {
        data_probe_id = pad->add_probe(Gst::PAD_PROBE_TYPE_DATA_DOWNSTREAM,
                                       sigc::ptr_fun(&on_sink_pad_have_data));
    }

    std::cout << "sink data probe id = " << data_probe_id << "\n";

    source->set_property("location", filename);

    // Get the bus from the pipeline, and add a bus watch to the default main
    // context with the default priority:
    ref<Gst::Bus> bus = pipeline->get_bus();
    bus->add_watch(sigc::ptr_fun(&on_bus_message));

    try {
        pipeline->add(source)->add(parser)->add(decoder)->add(conv)->add(sink);
    } catch(const Glib::Error& ex) {
        std::cerr << "Error while adding elements to the pipeline: "
                  << ex.what() << "\n";
        return 1;
    }

    try {
        source->link(parser);
        parser->signal_pad_added().connect(sigc::ptr_fun(&on_parser_pad_added));
        decoder->link(conv)->link(sink);
    } catch(const std::runtime_error& ex) {
        std::cout << "Exception while linking elements: " << ex.what() << "\n";
    }

    Glib::signal_timeout().connect(sigc::ptr_fun(&on_timeout), 200);

    std::cout << "Setting to PLAYING.\n";
    pipeline->set_state(Gst::STATE_PLAYING);
    std::cout << "Running.\n";
    mainloop->run();

    std::cout << "Returned. Stopping playback.\n";
    pipeline->set_state(Gst::STATE_NULL);

    return 0;
}
