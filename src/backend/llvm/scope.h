#ifndef CENT_BACKEND_SCOPE_H
#define CENT_BACKEND_SCOPE_H

#include <cstdint>
#include <map>
#include <string_view>

namespace cent::backend {

struct Type;
struct Value;

using TranslationUnit = std::uint16_t;

struct Scope {
    template <typename ElementType> struct Element {
        ElementType element;
        bool is_public{false};
        TranslationUnit unit{0};
    };

    template <typename ElementType>
    using NamesMap = std::map<std::string_view, Element<ElementType>>;

    NamesMap<Type*> types;
    NamesMap<Value> names;

    std::map<std::string_view, Scope> scopes;
};

} // namespace cent::backend

#endif
