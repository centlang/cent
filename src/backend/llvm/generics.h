#ifndef CENT_BACKEND_GENERICS_H
#define CENT_BACKEND_GENERICS_H

#include <map>
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

struct Scope;

struct GenericFunction {
    struct Param {
        std::string name;
        Type* type;
        bool is_mutable;
    };

    enum struct FnKind : std::uint8_t { Normal, AsSlice, AsMutSlice, Sizeof };

    OffsetValue<std::string> name;

    Type* return_type;

    std::vector<Param> params;
    std::vector<const ast::Expression*> default_args;

    const ast::BlockStmt* block{nullptr};

    std::vector<types::TypeParam*> type_params;
    std::vector<types::TypeParam*> parent_type_params;

    Type* self_type{nullptr};

    FnKind kind{FnKind::Normal};
    bool has_params;

    Scope* scope{nullptr};
    std::string source_file;
};

struct GenericStruct {
    struct Field {
        std::string name;
        Type* type;
    };

    std::string name;
    std::vector<Field> fields;

    std::vector<types::TypeParam*> type_params;

    std::map<std::string_view, GenericFunction> associated_fns;
};

struct GenericUnion {
    struct Field {
        std::string name;
        Type* type;
    };

    std::string name;
    std::vector<Field> fields;

    std::vector<types::TypeParam*> type_params;

    types::Enum* tag_type{nullptr};
    bool implicit{false};

    std::map<std::string_view, GenericFunction> associated_fns;
};

} // namespace cent::backend

#endif
