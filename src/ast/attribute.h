#ifndef CENT_AST_ATTRIBUTE_H
#define CENT_AST_ATTRIBUTE_H

#include <string>

namespace cent::ast {

struct Attribute {
    [[nodiscard]] Attribute(std::size_t offset, std::string name)
    : offset{offset}, name{std::move(name)} {}

    std::size_t offset;
    std::string name;
};

} // namespace cent::ast

#endif
