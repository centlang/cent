#ifndef CENT_BACKEND_SCOPE_H
#define CENT_BACKEND_SCOPE_H

#include <map>
#include <string_view>

#include "generics.h"

namespace cent::backend {

struct Type;
struct Value;

struct Scope {
    std::map<std::string_view, Type*> types;
    std::map<std::string_view, Value> names;
    std::map<std::string_view, Scope> scopes;

    std::map<std::string_view, std::shared_ptr<GenericStruct>> generic_structs;
    std::map<std::string_view, std::shared_ptr<GenericUnion>> generic_unions;
    std::map<std::string_view, std::shared_ptr<GenericFunction>> generic_fns;
};

} // namespace cent::backend

#endif
