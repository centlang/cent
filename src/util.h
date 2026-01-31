#ifndef CENT_UTIL_H
#define CENT_UTIL_H

#include <algorithm>
#include <cctype>
#include <fstream>
#include <limits>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

#include "log.h"

namespace cent {

[[nodiscard]] inline std::uint8_t
edit_distance(std::string_view first, std::string_view second) {
    std::vector distances(
        first.size() + 1, std::vector<std::uint8_t>(second.size() + 1));

    for (std::size_t i = 0; i <= first.size(); ++i) {
        distances[i][0] = i;
    }

    for (std::size_t i = 0; i <= second.size(); ++i) {
        distances[0][i] = i;
    }

    for (std::size_t i = 1; i <= first.size(); ++i) {
        for (std::size_t j = 1; j <= second.size(); ++j) {
            if (std::tolower(first[i - 1]) == std::tolower(second[j - 1])) {
                distances[i][j] = distances[i - 1][j - 1];
                continue;
            }

            distances[i][j] =
                1 + std::min(
                        std::min(distances[i][j - 1], distances[i - 1][j]),
                        distances[i - 1][j - 1]);
        }
    }

    return distances[first.size()][second.size()];
}

template <typename... Maps>
[[nodiscard]] inline std::optional<std::string_view>
closest_match(std::string_view name, const Maps&... maps) {
    static constexpr auto u8_max = std::numeric_limits<std::uint8_t>::max();
    static constexpr std::uint8_t max_threshold = 5;

    auto threshold =
        std::min(static_cast<std::uint8_t>(name.size() / 2), max_threshold);

    std::string_view result;
    std::uint8_t min_distance = u8_max;

    (
        [&] {
            for (const auto& pair : maps) {
                if (auto distance = edit_distance(name, pair.first);
                    distance <= threshold && distance < min_distance) {
                    result = pair.first;
                    min_distance = distance;
                }
            }
        }(),
        ...);

    if (min_distance == u8_max) {
        return std::nullopt;
    }

    return result;
}

[[nodiscard]] inline std::optional<std::string>
read_file(const std::string& filename) {
    std::ifstream file{filename};

    if (!file) {
        log::error(
            "could not open file {}: {}", log::quoted(filename),
            std::strerror(errno));

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

[[nodiscard]] std::filesystem::path get_exe_path();

[[nodiscard]] inline std::vector<std::string>
split(const std::string& value, char delimiter) {
    std::vector<std::string> result;
    std::string current;

    std::istringstream stream{value};

    while (std::getline(stream, current, delimiter)) {
        result.push_back(std::move(current));
    }

    return result;
}

} // namespace cent

#endif
