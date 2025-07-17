#ifndef CENT_BACKEND_SCOPE_H
#define CENT_BACKEND_SCOPE_H

#include <cstdint>
#include <map>
#include <string_view>

#include "generics.h"

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

    NamesMap<std::shared_ptr<GenericStruct>> generic_structs;
    NamesMap<std::shared_ptr<GenericUnion>> generic_unions;
    NamesMap<std::shared_ptr<GenericFunction>> generic_fns;

    std::map<std::string_view, Scope> scopes;
};

} // namespace cent::backend

#endif
