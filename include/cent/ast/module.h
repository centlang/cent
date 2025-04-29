#ifndef CENT_AST_MODULE_H
#define CENT_AST_MODULE_H

#include <map>
#include <memory>
#include <utility>
#include <vector>

#include "cent/ast/fn_decl.h"
#include "cent/ast/struct.h"

namespace cent::ast {

struct Module : Node {
    [[nodiscard]] Module(
        std::vector<std::unique_ptr<FnDecl>> functions = {}) noexcept
    : functions{std::move(functions)} {}

    std::vector<std::unique_ptr<FnDecl>> functions;
    std::vector<std::unique_ptr<Struct>> structs;

    std::map<std::string, std::unique_ptr<Module>> submodules;
};

} // namespace cent::ast

#endif
