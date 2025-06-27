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

[[nodiscard]] inline std::pair<std::uint32_t, std::uint32_t>
offset_to_pos(std::string_view source, std::size_t offset) {
    std::uint32_t line = 1;
    std::uint32_t column = 1;

    for (std::size_t i = 0; i < offset; ++i) {
        if (source[i] == '\n') {
            ++line;
            column = 1;
        } else {
            ++column;
        }
    }

    return {line, column};
}

[[nodiscard]] int
exec_command(std::string program, std::vector<std::string> args);

} // namespace cent

#endif
