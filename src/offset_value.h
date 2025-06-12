#ifndef CENT_AST_OFFSET_VALUE_H
#define CENT_AST_OFFSET_VALUE_H

#include <cstddef>

namespace cent::ast {

template <typename ValueType> struct OffsetValue {
    ValueType value;
    std::size_t offset;
};

} // namespace cent::ast

#endif
