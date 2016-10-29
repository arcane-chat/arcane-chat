#include "task.hpp"

#include <vector>
#include <string>

class database_task : public task {
    database_task();
    void run(std::vector<std::string> arguments);
}
