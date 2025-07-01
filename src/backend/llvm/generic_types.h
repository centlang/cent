#ifndef CENT_BACKEND_GENERIC_TYPES_H
#define CENT_BACKEND_GENERIC_TYPES_H

#include <string>

#include "backend/llvm/type.h"

#include "backend/llvm/types/enum.h"
#include "backend/llvm/types/template_param.h"

namespace cent::backend {

struct GenericStruct {
    struct Field {
        std::string name;
        Type* type;
    };

    std::string name;
    std::vector<Field> fields;

    std::vector<types::TemplateParam*> params;
};

struct GenericUnion {
    struct Field {
        std::string name;
        Type* type;
    };

    std::string name;
    std::vector<Field> fields;

    std::vector<types::TemplateParam*> params;
    types::Enum* tag_type;
};

} // namespace cent::backend

#endif
