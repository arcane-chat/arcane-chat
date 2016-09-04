#include <boost/algorithm/hex.hpp>
#include <cstdint>

#include "utils.hpp"

std::string to_hex(const std::vector<uint8_t> &bin) {
    std::string out;
    out.resize(bin.size() * 2);
    boost::algorithm::hex(bin.begin(), bin.end(), out.begin());
    return out;
}
