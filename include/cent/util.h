#ifndef CENT_UTIL_H
#define CENT_UTIL_H

#include <fstream>
#include <optional>
#include <sstream>
#include <string>

#include "cent/log.h"

namespace cent {

[[nodiscard]] inline std::optional<std::string>
read_file(const std::string& filename) noexcept {
    std::ifstream file{filename};

    if (!file) {
        cent::error(
            fmt::format("could not open file: {}", std::strerror(errno)));

        return std::nullopt;
    }

    std::ostringstream buffer;
    buffer << file.rdbuf();

    return buffer.str();
}

} // namespace cent

#endif
