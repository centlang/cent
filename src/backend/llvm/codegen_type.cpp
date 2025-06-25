#include <cstddef>

#include <llvm/IR/Constants.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>

#include "ast/module.h"

#include "ast/type/array_type.h"
#include "ast/type/fn_pointer.h"
#include "ast/type/named_type.h"
#include "ast/type/optional.h"
#include "ast/type/pointer.h"
#include "ast/type/range_type.h"
#include "ast/type/slice_type.h"
#include "ast/type/tuple_type.h"

#include "backend/llvm/type.h"
#include "backend/llvm/value.h"

#include "backend/llvm/types/alias.h"
#include "backend/llvm/types/enum.h"
#include "backend/llvm/types/function.h"
#include "backend/llvm/types/primitive.h"
#include "backend/llvm/types/struct.h"
#include "backend/llvm/types/union.h"

#include "backend/llvm/codegen.h"

namespace cent::backend {

Type* Codegen::generate(const ast::NamedType& type) {
    auto* scope = m_current_scope;
    std::size_t last_index = type.value.size() - 1;

    for (std::size_t i = 0; i < last_index; ++i) {
        scope = get_scope(type.value[i].offset, type.value[i].value, *scope);

        if (!scope) {
            return nullptr;
        }
    }

    return get_type(
        type.value[last_index].offset, type.value[last_index].value, *scope);
}

Type* Codegen::generate(const ast::Pointer& type) {
    auto* points_to = type.type->codegen(*this);

    if (!points_to) {
        return nullptr;
    }

    return get_ptr_type(points_to, type.is_mutable);
}

Type* Codegen::generate(const ast::Optional& type) {
    auto* contained = type.type->codegen(*this);

    if (!contained) {
        return nullptr;
    }

    return get_optional_type(contained);
}

Type* Codegen::generate(const ast::ArrayType& type) {
    auto* contained = type.type->codegen(*this);

    if (!contained) {
        return nullptr;
    }

    auto size = type.size->codegen(*this);

    if (!size) {
        return nullptr;
    }

    auto value = cast(m_primitive_types["usize"].get(), *size);

    if (!value) {
        return nullptr;
    }

    if (auto* constant = llvm::dyn_cast<llvm::ConstantInt>(value->value)) {
        return get_array_type(contained, constant->getZExtValue());
    }

    error(type.offset, "not a constant");

    return nullptr;
}

Type* Codegen::generate(const ast::SliceType& type) {
    auto* contained = type.type->codegen(*this);

    if (!contained) {
        return nullptr;
    }

    return get_slice_type(contained, type.is_mutable);
}

Type* Codegen::generate(const ast::TupleType& type) {
    std::vector<Type*> types;
    types.reserve(type.types.size());

    for (const auto& element_type : type.types) {
        auto* el_type = element_type->codegen(*this);

        if (!el_type) {
            return nullptr;
        }

        types.push_back(el_type);
    }

    return get_tuple_type(types);
}

Type* Codegen::generate(const ast::RangeType& type) {
    auto* contained = type.type->codegen(*this);

    if (!contained) {
        return nullptr;
    }

    return get_range_type(contained);
}

Type* Codegen::generate(const ast::FnPointer& type) {
    return generate_fn_type(type.proto);
}

types::Function* Codegen::generate_fn_type(const ast::FnProto& proto) {
    Type* return_type = m_void_type.get();

    if (proto.return_type) {
        return_type = proto.return_type->codegen(*this);

        if (!return_type) {
            return nullptr;
        }
    }

    std::vector<Type*> param_types;
    std::vector<llvm::Constant*> default_args;

    param_types.reserve(proto.params.size());

    for (const auto& parameter : proto.params) {
        auto* type = parameter.type->codegen(*this);

        if (!type) {
            return nullptr;
        }

        param_types.push_back(type);

        if (!parameter.value) {
            continue;
        }

        auto value = parameter.value->codegen(*this);

        if (!value) {
            return nullptr;
        }

        auto val = cast(type, *value);

        if (!val) {
            type_mismatch(parameter.value->offset, type, value->type);
            return nullptr;
        }

        if (!llvm::isa<llvm::Constant>(val->value)) {
            error(parameter.value->offset, "not a constant");
            return nullptr;
        }

        default_args.push_back(static_cast<llvm::Constant*>(val->value));
    }

    return get_fn_type(
        return_type, std::move(param_types), std::move(default_args),
        proto.variadic);
}

types::Function* Codegen::get_fn_type(
    Type* return_type, std::vector<Type*> param_types,
    std::vector<llvm::Constant*> default_args, bool variadic) {
    auto& result = m_fn_types[std::make_tuple(
        return_type, param_types, default_args, variadic)];

    if (result) {
        return result.get();
    }

    std::vector<llvm::Type*> llvm_param_types;
    llvm_param_types.reserve(param_types.size());

    for (const auto& parameter : param_types) {
        llvm_param_types.push_back(parameter->llvm_type);
    }

    auto* llvm_type = llvm::FunctionType::get(
        return_type->llvm_type, llvm_param_types, variadic);

    result = std::make_unique<types::Function>(
        llvm_type, return_type, std::move(param_types), std::move(default_args),
        variadic);

    return result.get();
}

types::Pointer* Codegen::get_ptr_type(Type* type, bool is_mutable) {
    auto& result = m_ptr_types[std::make_pair(type, is_mutable)];

    if (!result) {
        result = std::make_unique<types::Pointer>(
            llvm::PointerType::get(m_context, 0), type, is_mutable);
    }

    return result.get();
}

types::Slice* Codegen::get_slice_type(Type* type, bool is_mutable) {
    auto& result = m_slice_types[std::make_pair(type, is_mutable)];

    if (!result) {
        result = std::make_unique<types::Slice>(m_slice_type, type, is_mutable);
    }

    return result.get();
}

types::Array* Codegen::get_array_type(Type* type, std::size_t size) {
    auto& result = m_array_types[std::make_pair(type, size)];

    if (!result) {
        result = std::make_unique<types::Array>(
            llvm::ArrayType::get(type->llvm_type, size), type, size);
    }

    return result.get();
}

types::Tuple* Codegen::get_tuple_type(const std::vector<Type*>& types) {
    auto& result = m_tuple_types[types];

    if (!result) {
        std::vector<llvm::Type*> llvm_types;
        llvm_types.reserve(types.size());

        for (auto* element_type : types) {
            llvm_types.push_back(element_type->llvm_type);
        }

        result = std::make_unique<types::Tuple>(
            llvm::StructType::create(llvm_types), types);
    }

    return result.get();
}

types::Optional* Codegen::get_optional_type(Type* type) {
    auto& result = m_optional_types[type];

    if (!result) {
        if (is<types::Pointer>(type)) {
            result = std::make_unique<types::Optional>(type->llvm_type, type);
        } else {
            auto* llvm_type = llvm::StructType::create(
                {type->llvm_type, llvm::Type::getInt1Ty(m_context)});

            result = std::make_unique<types::Optional>(llvm_type, type);
        }
    }

    return result.get();
}

types::Range* Codegen::get_range_type(Type* type) {
    auto& result = m_range_types[type];

    if (!result) {
        auto* llvm_type =
            llvm::StructType::create({type->llvm_type, type->llvm_type});

        result = std::make_unique<types::Range>(llvm_type, type);
    }

    return result.get();
}

} // namespace cent::backend
