#ifndef CENT_POSITION_H
#define CENT_POSITION_H

#include <cstdint>

namespace cent {

struct Position {
    std::uint32_t line = 1;
    std::uint32_t column = 1;
};

} // namespace cent

#endif
