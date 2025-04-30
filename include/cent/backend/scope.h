#ifndef CENT_BACKEND_SCOPE_H
#define CENT_BACKEND_SCOPE_H

#include <map>
#include <memory>
#include <string_view>

namespace cent::backend {

struct Type;
struct Value;

struct Scope {
    std::map<std::string_view, std::shared_ptr<Type>> types;
    std::map<std::string_view, Value> names;

    std::map<std::string_view, Scope> scopes;
};

} // namespace cent::backend

#endif
