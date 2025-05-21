#ifndef CENT_AST_MODULE_H
#define CENT_AST_MODULE_H

#include <map>
#include <memory>
#include <utility>
#include <vector>

#include "cent/modules.h"

#include "cent/ast/enum_decl.h"
#include "cent/ast/fn_decl.h"
#include "cent/ast/struct.h"
#include "cent/ast/var_decl.h"

namespace cent::ast {

struct Module : Node {
    [[nodiscard]] Module(ModulePath path) noexcept : path{std::move(path)} {};

    std::vector<std::unique_ptr<FnDecl>> functions;
    std::vector<std::unique_ptr<Struct>> structs;
    std::vector<std::unique_ptr<EnumDecl>> enums;
    std::vector<std::unique_ptr<VarDecl>> variables;
    std::map<std::string, std::unique_ptr<Module>> submodules;

    ModulePath path;
};

} // namespace cent::ast

#endif
