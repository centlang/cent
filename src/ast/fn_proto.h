#ifndef CENT_AST_FN_PROTO_H
#define CENT_AST_FN_PROTO_H

#include <memory>
#include <string>
#include <vector>

#include "ast/node.h"
#include "offset_value.h"

namespace cent::ast {

struct FnProto {
    struct Param {
        OffsetValue<std::string> name;
        std::unique_ptr<Type> type;

        std::unique_ptr<Expression> value;
        bool is_mutable;
    };

    std::vector<Param> params;
    std::unique_ptr<Type> return_type;

    bool variadic;
};

} // namespace cent::ast

#endif
