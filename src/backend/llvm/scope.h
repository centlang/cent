#ifndef CENT_BACKEND_SCOPE_H
#define CENT_BACKEND_SCOPE_H

#include <map>
#include <string_view>

#include "generic_types.h"

namespace cent::backend {

struct Type;
struct Value;

struct Scope {
    std::map<std::string_view, Type*> types;
    std::map<std::string_view, Value> names;
    std::map<std::string_view, Scope> scopes;

    std::map<std::string_view, GenericStruct> generic_structs;
    std::map<std::string_view, GenericUnion> generic_unions;
};

} // namespace cent::backend

#endif
