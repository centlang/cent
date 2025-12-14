#ifndef CENT_AST_MODULE_H
#define CENT_AST_MODULE_H

#include <filesystem>
#include <memory>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "ast/decl/fn_decl.h"
#include "ast/decl/var_decl.h"
#include "ast/node.h"

namespace cent::ast {

struct Module : Node {
    [[nodiscard]] Module(
        std::filesystem::path path,
        std::optional<std::string> name = std::nullopt)
    : path{std::move(path)}, name{std::move(name)} {};

    std::vector<std::unique_ptr<Declaration>> types;
    std::vector<std::unique_ptr<FnDecl>> functions;
    std::vector<std::unique_ptr<VarDecl>> variables;

    std::vector<std::unique_ptr<Module>> imported_modules;
    std::vector<std::unique_ptr<Module>> submodules;

    std::filesystem::path path;
    std::optional<std::string> name;
};

} // namespace cent::ast

#endif
