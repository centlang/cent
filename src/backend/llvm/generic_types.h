#ifndef CENT_BACKEND_GENERIC_TYPES_H
#define CENT_BACKEND_GENERIC_TYPES_H

#include <map>
#include <memory>
#include <string>
#include <vector>

#include "backend/llvm/type.h"
#include "backend/llvm/types/template_param.h"

namespace cent::backend {

struct GenericStruct {
    std::string name;
    std::vector<Type*> fields;

    std::map<std::string, types::TemplateParam*> params;
};

struct GenericUnion {
    std::string name;
    std::vector<Type*> fields;

    std::map<std::string, types::TemplateParam*> params;
};

} // namespace cent::backend

#endif
