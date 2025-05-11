#ifndef CENT_LOG_H
#define CENT_LOG_H

#include <cstdint>
#include <string_view>

#include <fmt/core.h>

namespace cent::log {

inline void
log(std::string_view type, std::uint32_t line, std::uint32_t column,
    std::string_view filename, std::string_view message) noexcept {
    fmt::print(
        stderr, "{}:{}:{}: {}: {}\n", filename, line, column, type, message);
}

inline void log(std::string_view type, std::string_view message) noexcept {
    fmt::print(stderr, "{}: {}\n", type, message);
}

inline void error(
    std::uint32_t line, std::uint32_t column, std::string_view filename,
    std::string_view message) noexcept {
    log("error", line, column, filename, message);
}

inline void error(std::string_view message) noexcept { log("error", message); }

inline void warning(
    std::uint32_t line, std::uint32_t column, std::string_view filename,
    std::string_view message) noexcept {
    log("warning", line, column, filename, message);
}

inline void warning(std::string_view message) noexcept {
    log("warning", message);
}

inline void note(
    std::uint32_t line, std::uint32_t column, std::string_view filename,
    std::string_view message) noexcept {
    log("note", line, column, filename, message);
}

inline void note(std::string_view message) noexcept { log("note", message); }

} // namespace cent::log

#endif
