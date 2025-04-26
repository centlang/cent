#ifndef CENT_LOG_H
#define CENT_LOG_H

#include <string_view>

#include <fmt/core.h>

#include "cent/position.h"

namespace cent {

inline void
log(std::string_view type, Position position, std::string_view filename,
    std::string_view message) noexcept {
    fmt::print(
        stderr, "{}:{}:{}: {}: {}\n", filename, position.line, position.column,
        type, message);
}

inline void log(std::string_view type, std::string_view message) noexcept {
    fmt::print(stderr, "{}: {}\n", type, message);
}

inline void error(
    Position position, std::string_view filename,
    std::string_view message) noexcept {
    log("error", position, filename, message);
}

inline void error(std::string_view message) noexcept { log("error", message); }

inline void warning(
    Position position, std::string_view filename,
    std::string_view message) noexcept {
    log("warning", position, filename, message);
}

inline void warning(std::string_view message) noexcept {
    log("warning", message);
}

inline void note(
    Position position, std::string_view filename,
    std::string_view message) noexcept {
    log("note", position, filename, message);
}

inline void note(std::string_view message) noexcept { log("note", message); }

} // namespace cent

#endif
