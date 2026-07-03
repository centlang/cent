#ifndef CENT_BACKEND_GENERICS_H
#define CENT_BACKEND_GENERICS_H

#include <string>
#include <vector>

#include "offset_value.h"

#include "backend/llvm/type.h"
#include "backend/llvm/types/enum.h"
#include "backend/llvm/types/type_param.h"

namespace cent::ast {

struct BlockStmt;
struct Expression;

} // namespace cent::ast

namespace cent::backend {

struct GenericStruct {
    struct Field {
        std::string name;
        Type* type;
    };

    std::string name;
    std::vector<Field> fields;

    std::vector<types::TypeParam*> template_params;
};

struct GenericUnion {
    struct Field {
        std::string name;
        Type* type;
    };

    std::string name;
    std::vector<Field> fields;

    std::vector<types::TypeParam*> template_params;
    types::Enum* tag_type{nullptr};
};

struct GenericFunction {
    struct Param {
        std::string name;
        Type* type;
        bool is_mutable;
    };

    OffsetValue<std::string> name;

    Type* return_type;

    std::vector<Param> params;
    std::vector<const ast::Expression*> default_args;

    const ast::BlockStmt* block{nullptr};

    std::vector<types::TypeParam*> template_params;
};

} // namespace cent::backend

#endif
