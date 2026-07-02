#ifndef CENT_AST_ATTRIBUTE_H
#define CENT_AST_ATTRIBUTE_H

#include <optional>
#include <string>

#include "offset_value.h"

namespace cent::ast {

struct Attribute {
    OffsetValue<std::string> name;
    std::optional<std::string> value;
};

} // namespace cent::ast

#endif
