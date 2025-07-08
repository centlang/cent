#ifndef CENT_BACKEND_GENERIC_TYPES_H
#define CENT_BACKEND_GENERIC_TYPES_H

#include <string>

#include "backend/llvm/type.h"
#include "backend/llvm/types/enum.h"

namespace cent::ast {

struct Expression;
struct BlockStmt;

} // namespace cent::ast

namespace cent::backend {

namespace types {

struct TemplateParam;

} // namespace types

struct GenericStruct {
    struct Field {
        std::string name;
        Type* type;
    };

    std::string mangled_name;
    std::vector<Field> fields;

    std::vector<types::TemplateParam*> template_params;
};

struct GenericUnion {
    struct Field {
        std::string name;
        Type* type;
    };

    std::string mangled_name;
    std::vector<Field> fields;

    std::vector<types::TemplateParam*> template_params;
    types::Enum* tag_type;
};

struct GenericFunction {
    struct Param {
        std::string name;
        Type* type;
        bool is_mutable;
    };

    std::string mangled_name;
    std::size_t name_offset;

    Type* return_type;
    std::vector<Param> params;
    std::vector<const ast::Expression*> default_args;

    const ast::BlockStmt* block;

    std::vector<types::TemplateParam*> template_params;
};

} // namespace cent::backend

#endif
