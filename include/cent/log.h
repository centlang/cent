#ifndef CENT_LOG_H
#define CENT_LOG_H

#include <cstdint>
#include <string_view>

#include <fmt/format.h>

#define CENT_ANSI_CSI "\x1b["
#define CENT_ANSI_RESET CENT_ANSI_CSI "0m"
#define CENT_ANSI_BOLD '1'
#define CENT_ANSI_ITALIC '3'
#define CENT_ANSI_UNDERLINE '4'
#define CENT_ANSI_STRIKETHROUGH '9'
#define CENT_ANSI_COLOR_FG '3'
#define CENT_ANSI_COLOR_BG '4'

namespace cent::log {

enum Mode : std::uint8_t {
    Bold = 1 << 0,
    Italic = 1 << 1,
    Underline = 1 << 2,
    Strikethrough = 1 << 3,
};

enum Color : std::uint8_t {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
    Default = 9
};

template <typename ValueType> struct Styled {
    ValueType value;

    Color fg = Default;
    Color bg = Default;

    std::uint8_t mode = 0;
};

} // namespace cent::log

template <typename ValueType>
struct fmt::formatter<cent::log::Styled<ValueType>> {
    constexpr auto parse(fmt::format_parse_context& context) noexcept {
        return context.begin();
    }

    template <typename Context>
    constexpr auto format(
        cent::log::Styled<ValueType> styled, Context& context) const noexcept {
        std::string style;
        bool put_semicolon = false;

        auto add_separator = [&] {
            if (put_semicolon) {
                style += ';';
            }
        };

        if (styled.mode != 0 || styled.fg != cent::log::Default ||
            styled.bg != cent::log::Default) {
            style = CENT_ANSI_CSI;
        }

        if (styled.mode & cent::log::Bold) {
            style += CENT_ANSI_BOLD;
            put_semicolon = true;
        }

        if (styled.mode & cent::log::Italic) {
            add_separator();
            style += CENT_ANSI_ITALIC;
        }

        if (styled.mode & cent::log::Underline) {
            add_separator();
            style += CENT_ANSI_UNDERLINE;
        }

        if (styled.mode & cent::log::Strikethrough) {
            add_separator();
            style += CENT_ANSI_STRIKETHROUGH;
        }

        if (styled.fg != cent::log::Default) {
            add_separator();

            style += CENT_ANSI_COLOR_FG;
            style += '0' + styled.fg;
        }

        if (styled.bg != cent::log::Default) {
            add_separator();

            style += CENT_ANSI_COLOR_BG;
            style += '0' + styled.bg;
        }

        if (!style.empty()) {
            style += 'm';
        }

        return fmt::format_to(
            context.out(), "{}{}{}", style, styled.value,
            style.empty() ? "" : CENT_ANSI_RESET);
    }
};

namespace cent::log {

template <typename ValueType>
inline auto fg(ValueType value, Color color) noexcept {
    return Styled{.value = value, .fg = color};
}

template <typename ValueType>
inline auto bg(ValueType value, Color color) noexcept {
    return Styled{.value = value, .bg = color};
}

template <typename ValueType> inline auto bold(ValueType value) noexcept {
    return Styled{.value = value, .mode = Bold};
}

template <typename ValueType> inline auto italic(ValueType value) noexcept {
    return Styled{.value = value, .mode = Italic};
}

template <typename ValueType> inline auto underline(ValueType value) noexcept {
    return Styled{.value = value, .mode = Underline};
}

template <typename ValueType>
inline auto strikethrough(ValueType value) noexcept {
    return Styled{.value = value, .mode = Underline};
}

template <typename ValueType>
inline auto fg(Styled<ValueType> value, Color color) noexcept {
    value.fg = color;
    return value;
}

template <typename ValueType>
inline auto bg(Styled<ValueType> value, Color color) noexcept {
    value.bg = color;
    return value;
}

template <typename ValueType>
inline auto mode(Styled<ValueType> value, Mode mode) noexcept {
    value.mode |= mode;
    return value;
}

template <typename ValueType>
inline auto bold(Styled<ValueType> value) noexcept {
    return mode(value, Bold);
}

template <typename ValueType>
inline auto italic(Styled<ValueType> value) noexcept {
    return mode(value, Italic);
}

template <typename ValueType>
inline auto underline(Styled<ValueType> value) noexcept {
    return mode(value, Underline);
}

template <typename ValueType>
inline auto strikethrough(Styled<ValueType> value) noexcept {
    return mode(value, Strikethrough);
}

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
