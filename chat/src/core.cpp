#include "core.hpp"

using namespace chat;

Core::Core(Tox *tox) : tox(tox) {
    iterator.setSingleShot(true);
    connect(&iterator,SIGNAL(timeout()),this,SLOT(checkTox()));
    iterator.setInterval(tox_iteration_interval(tox));
    iterator.start();
}
void Core::checkTox() {
    tox_iterate(tox);
    // ^^^ will call the callback functions defined and registered
    iterator.setInterval(tox_iteration_interval(tox));
    iterator.start();
}
