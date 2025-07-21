#ifndef CENT_OFFSET_VALUE_H
#define CENT_OFFSET_VALUE_H

#include <cstddef>

namespace cent {

template <typename ValueType> struct OffsetValue {
    ValueType value;
    std::size_t offset;
};

} // namespace cent

#endif
