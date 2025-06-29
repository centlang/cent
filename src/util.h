#ifndef CENT_UTIL_H
#define CENT_UTIL_H

#include <fstream>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "log.h"

namespace cent {

[[nodiscard]] inline std::optional<std::string>
read_file(const std::string& filename) {
    std::ifstream file{filename};

    if (!file) {
        log::error(fmt::format(
            "could not open file `{}`: {}", filename, std::strerror(errno)));

        return std::nullopt;
    }

    std::ostringstream buffer;
    buffer << file.rdbuf();

    return buffer.str();
}

struct Location {
    std::uint32_t line = 1;
    std::uint32_t column = 1;

    std::string code;
};

[[nodiscard]] inline Location
offset_to_loc(std::string_view source, std::size_t offset) {
    Location location;
    std::size_t line_begin = 0;

    for (std::size_t i = 0; i < offset; ++i) {
        if (source[i] == '\n') {
            ++location.line;
            line_begin = i + 1;
            location.column = 1;
        } else {
            ++location.column;
        }
    }

    std::size_t line_end = source.find('\n', line_begin);

    if (line_end == std::string_view::npos) {
        location.code = source.substr(line_begin);
        return location;
    }

    location.code = source.substr(line_begin, line_end - line_begin);
    return location;
}

[[nodiscard]] int
exec_command(std::string program, std::vector<std::string> args);

} // namespace cent

#endif
