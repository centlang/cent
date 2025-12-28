#ifndef CENT_UNICODE_H
#define CENT_UNICODE_H

#include <cstdint>
#include <string>

namespace cent {

inline void append_utf8(std::string& string, std::uint32_t codepoint) {
    if (codepoint <= 0x7f) {
        string += static_cast<char>(codepoint);
        return;
    }

    if (codepoint <= 0x7ff) {
        string += static_cast<char>(0xc0 | (codepoint >> 6));
        string += static_cast<char>(0x80 | (codepoint & 0x3f));
        return;
    }

    if (codepoint <= 0xffff) {
        string += static_cast<char>(0xe0 | (codepoint >> 12));
        string += static_cast<char>(0x80 | ((codepoint >> 6) & 0x3f));
        string += static_cast<char>(0x80 | (codepoint & 0x3f));
        return;
    }

    string += static_cast<char>(0xf0 | (codepoint >> 18));
    string += static_cast<char>(0x80 | ((codepoint >> 12) & 0x3f));
    string += static_cast<char>(0x80 | ((codepoint >> 6) & 0x3f));
    string += static_cast<char>(0x80 | (codepoint & 0x3f));
}

inline std::uint32_t get_utf8(const std::string& utf8_char) {
    auto byte1 = static_cast<unsigned char>(utf8_char[0]);

    if (byte1 <= 0x7f) {
        return static_cast<std::uint32_t>(byte1);
    }

    auto byte2 = static_cast<unsigned char>(utf8_char[1]);

    if ((byte1 & 0xe0) == 0xc0) {
        return (static_cast<std::uint32_t>(byte1 & 0x1f) << 6) |
               (static_cast<std::uint32_t>(byte2 & 0x3f));
    }

    auto byte3 = static_cast<unsigned char>(utf8_char[2]);

    if ((byte1 & 0xf0) == 0xe0) {
        return (static_cast<std::uint32_t>(byte1 & 0x0f) << 12) |
               (static_cast<std::uint32_t>(byte2 & 0x3f) << 6) |
               (static_cast<std::uint32_t>(byte3 & 0x3f));
    }

    auto byte4 = static_cast<unsigned char>(utf8_char[3]);

    return (static_cast<std::uint32_t>(byte1 & 0x07) << 18) |
           (static_cast<std::uint32_t>(byte2 & 0x3f) << 12) |
           (static_cast<std::uint32_t>(byte3 & 0x3f) << 6) |
           (static_cast<std::uint32_t>(byte4 & 0x3f));
}

} // namespace cent

#endif
