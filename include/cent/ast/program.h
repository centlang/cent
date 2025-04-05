#ifndef CENT_AST_PROGRAM_H
#define CENT_AST_PROGRAM_H

#include <memory>
#include <utility>
#include <vector>

#include "cent/ast/fn_decl.h"
#include "cent/ast/struct.h"

namespace cent::ast {

struct Program : Node {
    [[nodiscard]] Program(
        std::vector<std::unique_ptr<FnDecl>> functions = {}) noexcept
    : functions{std::move(functions)} {}

    std::vector<std::unique_ptr<FnDecl>> functions;
    std::vector<std::unique_ptr<Struct>> structs;
};

} // namespace cent::ast

#endif
