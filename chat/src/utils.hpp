#pragma once

#include <vector>
#include <string>

//! Print a hexadecimal array.
std::string to_hex(const std::vector<uint8_t> &bin);

inline std::string to_hex(const uint8_t* bin_arr, size_t bin_size) {
    const std::vector<uint8_t> vec { bin_arr, bin_arr + bin_size };
    return to_hex(vec);
}
