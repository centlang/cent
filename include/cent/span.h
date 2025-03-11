#ifndef CENT_SPAN_H
#define CENT_SPAN_H

#include <cstdint>

#include "cent/position.h"

namespace cent {

struct Span {
    [[nodiscard]] static Span
    from(Position begin, std::uint32_t column_offset = 1) noexcept {
        return {begin, {begin.line, begin.column + column_offset}};
    }

    Position begin;
    Position end;
};

} // namespace cent

#endif
