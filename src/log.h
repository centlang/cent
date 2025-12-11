#ifndef CENT_LOG_H
#define CENT_LOG_H

#include <cstdint>
#include <string_view>

#include <fmt/format.h>

#include "options.h"

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
    Black = 0,
    Red = 1,
    Green = 2,
    Yellow = 3,
    Blue = 4,
    Magenta = 5,
    Cyan = 6,
    White = 7,
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
    constexpr auto parse(fmt::format_parse_context& context) {
        return context.begin();
    }

    template <typename Context>
    constexpr auto
    format(cent::log::Styled<ValueType> styled, Context& context) const {
        if (!cent::g_options.colorize) {
            return fmt::format_to(context.out(), "{}", styled.value);
        }

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
inline auto fg(const ValueType& value, Color color) {
    return Styled{.value = value, .fg = color};
}

template <typename ValueType>
inline auto bg(const ValueType& value, Color color) {
    return Styled{.value = value, .bg = color};
}

template <typename ValueType> inline auto bold(const ValueType& value) {
    return Styled{.value = value, .mode = Bold};
}

template <typename ValueType> inline auto italic(const ValueType& value) {
    return Styled{.value = value, .mode = Italic};
}

template <typename ValueType> inline auto underline(const ValueType& value) {
    return Styled{.value = value, .mode = Underline};
}

template <typename ValueType>
inline auto strikethrough(const ValueType& value) {
    return Styled{.value = value, .mode = Underline};
}

template <typename ValueType>
inline auto fg(Styled<ValueType> value, Color color) {
    value.fg = color;
    return value;
}

template <typename ValueType>
inline auto bg(Styled<ValueType> value, Color color) {
    value.bg = color;
    return value;
}

template <typename ValueType>
inline auto mode(Styled<ValueType> value, Mode mode) {
    value.mode |= mode;
    return value;
}

template <typename ValueType> inline auto bold(const Styled<ValueType>& value) {
    return mode(value, Bold);
}

template <typename ValueType>
inline auto italic(const Styled<ValueType>& value) {
    return mode(value, Italic);
}

template <typename ValueType>
inline auto underline(const Styled<ValueType>& value) {
    return mode(value, Underline);
}

template <typename ValueType>
inline auto strikethrough(const Styled<ValueType>& value) {
    return mode(value, Strikethrough);
}

template <typename ValueType> inline auto quoted(const ValueType& value) {
    return fmt::format("`{}`", value);
}

template <typename... Args>
inline void log_hint(
    std::string_view type, Color type_fg, std::uint32_t line,
    std::uint32_t column, std::string_view filename, std::string_view code,
    std::string_view hint, fmt::format_string<Args...> message,
    Args&&... args) {
    fmt::print(
        stderr, "{} {} {}\n",
        bold(fmt::format("{}:{}:{}:", filename, line, column)),
        bold(fg(fmt::format("{}:", type), type_fg)),
        bold(fmt::format(message, std::forward<Args>(args)...)));

    fmt::print(stderr, " {} | {}\n", line, code);

    fmt::print(
        stderr, " {:{}} |{:{}}", "", fmt::formatted_size("{}", line), "",
        column);

    fmt::print(
        stderr, "{}\n\n", bold(fg(fmt::format("^ hint: {}", hint), Green)));
}

template <typename... Args>
inline void
log(std::string_view type, Color type_fg, std::uint32_t line,
    std::uint32_t column, std::string_view filename, std::string_view code,
    fmt::format_string<Args...> message, Args&&... args) {
    fmt::print(
        stderr, "{} {} {}\n",
        bold(fmt::format("{}:{}:{}:", filename, line, column)),
        bold(fg(fmt::format("{}:", type), type_fg)),
        bold(fmt::format(message, std::forward<Args>(args)...)));

    fmt::print(stderr, " {} | {}\n", line, code);

    fmt::print(
        stderr, " {:{}} |{:{}}", "", fmt::formatted_size("{}", line), "",
        column);

    fmt::print(stderr, "{}\n\n", bold(fg("^", Green)));
}

template <typename... Args>
inline void
log(std::string_view type, Color type_fg, fmt::format_string<Args...> message,
    Args&&... args) {
    fmt::print(
        stderr, "{} {}\n\n", bold(fg(fmt::format("{}:", type), type_fg)),
        bold(fmt::format(message, std::forward<Args>(args)...)));
}

template <typename... Args>
inline void error_hint(
    std::uint32_t line, std::uint32_t column, std::string_view filename,
    std::string_view code, std::string_view hint,
    fmt::format_string<Args...> message, Args&&... args) {
    log_hint(
        "error", Red, line, column, filename, code, hint, message,
        std::forward<Args>(args)...);
}

template <typename... Args>
inline void error(
    std::uint32_t line, std::uint32_t column, std::string_view filename,
    std::string_view code, fmt::format_string<Args...> message,
    Args&&... args) {
    log("error", Red, line, column, filename, code, message,
        std::forward<Args>(args)...);
}

template <typename... Args>
inline void error(fmt::format_string<Args...> message, Args&&... args) {
    log("error", Red, message, std::forward<Args>(args)...);
}

template <typename... Args>
inline void warning_hint(
    std::uint32_t line, std::uint32_t column, std::string_view filename,
    std::string_view code, std::string_view hint,
    fmt::format_string<Args...> message, Args&&... args) {
    log_hint(
        "warning", Yellow, line, column, filename, code, hint, message,
        std::forward<Args>(args)...);
}

template <typename... Args>
inline void warning(
    std::uint32_t line, std::uint32_t column, std::string_view filename,
    std::string_view code, fmt::format_string<Args...> message,
    Args&&... args) {
    log("warning", Yellow, line, column, filename, code, message,
        std::forward<Args>(args)...);
}

template <typename... Args>
inline void warning(fmt::format_string<Args...> message, Args&&... args) {
    log("warning", Yellow, message, std::forward<Args>(args)...);
}

template <typename... Args>
inline void note_hint(
    std::uint32_t line, std::uint32_t column, std::string_view filename,
    std::string_view code, std::string_view hint,
    fmt::format_string<Args...> message, Args&&... args) {
    log_hint(
        "note", Cyan, line, column, filename, code, hint, message,
        std::forward<Args>(args)...);
}

template <typename... Args>
inline void note(
    std::uint32_t line, std::uint32_t column, std::string_view filename,
    std::string_view code, fmt::format_string<Args...> message,
    Args&&... args) {
    log("note", Cyan, line, column, filename, code, message,
        std::forward<Args>(args)...);
}

template <typename... Args>
inline void note(fmt::format_string<Args...> message, Args&&... args) {
    log("note", Cyan, message, std::forward<Args>(args)...);
}

template <typename... Args>
inline void verbose_hint(
    std::uint32_t line, std::uint32_t column, std::string_view filename,
    std::string_view code, std::string_view hint,
    fmt::format_string<Args...> message, Args&&... args) {
    log_hint(
        "verbose", Magenta, line, column, filename, code, hint, message,
        std::forward<Args>(args)...);
}

template <typename... Args>
inline void verbose(
    std::uint32_t line, std::uint32_t column, std::string_view filename,
    std::string_view code, fmt::format_string<Args...> message,
    Args&&... args) {
    log("verbose", Magenta, line, column, filename, code, message,
        std::forward<Args>(args)...);
}

template <typename... Args>
inline void verbose(fmt::format_string<Args...> message, Args&&... args) {
    if (cent::g_options.verbose) {
        log("info", Magenta, message, std::forward<Args>(args)...);
    }
}

} // namespace cent::log

#endif
