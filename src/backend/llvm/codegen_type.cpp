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

std::shared_ptr<Type> Codegen::generate(const ast::NamedType& type) {
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

std::shared_ptr<Type> Codegen::generate(const ast::Pointer& type) {
    auto points_to = type.type->codegen(*this);

    if (!points_to) {
        return nullptr;
    }

    return std::make_shared<types::Pointer>(points_to, type.is_mutable);
}

std::shared_ptr<Type> Codegen::generate(const ast::Optional& type) {
    auto contained = type.type->codegen(*this);

    if (!contained) {
        return nullptr;
    }

    return std::make_shared<types::Optional>(contained);
}

std::shared_ptr<Type> Codegen::generate(const ast::ArrayType& type) {
    auto contained = type.type->codegen(*this);

    if (!contained) {
        return nullptr;
    }

    auto size = type.size->codegen(*this);

    if (!size) {
        return nullptr;
    }

    auto value = cast(m_primitive_types["usize"], *size);

    if (!value) {
        return nullptr;
    }

    if (auto* constant = llvm::dyn_cast<llvm::ConstantInt>(value->value)) {
        return std::make_shared<types::Array>(
            contained, constant->getZExtValue());
    }

    error(type.offset, "not a constant");

    return nullptr;
}

std::shared_ptr<Type> Codegen::generate(const ast::SliceType& type) {
    auto contained = type.type->codegen(*this);

    if (!contained) {
        return nullptr;
    }

    return std::make_shared<types::Slice>(contained, type.is_mutable);
}

std::shared_ptr<Type> Codegen::generate(const ast::TupleType& type) {
    std::vector<std::shared_ptr<backend::Type>> types;
    std::vector<llvm::Type*> llvm_types;

    types.reserve(type.types.size());
    llvm_types.reserve(type.types.size());

    for (const auto& element_type : type.types) {
        auto el_type = element_type->codegen(*this);

        if (!el_type) {
            return nullptr;
        }

        types.push_back(std::move(el_type));
    }

    for (auto& element_type : types) {
        llvm_types.push_back(element_type->codegen(*this));
    }

    return std::make_shared<types::Tuple>(
        llvm::StructType::create(llvm_types), std::move(types));
}

std::shared_ptr<Type> Codegen::generate(const ast::RangeType& type) {
    auto contained = type.type->codegen(*this);

    if (!contained) {
        return nullptr;
    }

    return std::make_shared<types::Range>(contained);
}

std::shared_ptr<Type> Codegen::generate(const ast::FnPointer& type) {
    return generate_fn_type(type.proto);
}

llvm::Type* Codegen::generate([[maybe_unused]] const types::I8& type) {
    return llvm::Type::getInt8Ty(m_context);
}

llvm::Type* Codegen::generate([[maybe_unused]] const types::I16& type) {
    return llvm::Type::getInt16Ty(m_context);
}

llvm::Type* Codegen::generate([[maybe_unused]] const types::I32& type) {
    return llvm::Type::getInt32Ty(m_context);
}

llvm::Type* Codegen::generate([[maybe_unused]] const types::I64& type) {
    return llvm::Type::getInt64Ty(m_context);
}

llvm::Type* Codegen::generate([[maybe_unused]] const types::ISize& type) {
    return m_module->getDataLayout().getIntPtrType(m_context);
}

llvm::Type* Codegen::generate([[maybe_unused]] const types::U8& type) {
    return llvm::Type::getInt8Ty(m_context);
}

llvm::Type* Codegen::generate([[maybe_unused]] const types::U16& type) {
    return llvm::Type::getInt16Ty(m_context);
}

llvm::Type* Codegen::generate([[maybe_unused]] const types::U32& type) {
    return llvm::Type::getInt32Ty(m_context);
}

llvm::Type* Codegen::generate([[maybe_unused]] const types::U64& type) {
    return llvm::Type::getInt64Ty(m_context);
}

llvm::Type* Codegen::generate([[maybe_unused]] const types::USize& type) {
    return m_module->getDataLayout().getIntPtrType(m_context);
}

llvm::Type* Codegen::generate([[maybe_unused]] const types::F32& type) {
    return llvm::Type::getFloatTy(m_context);
}

llvm::Type* Codegen::generate([[maybe_unused]] const types::F64& type) {
    return llvm::Type::getDoubleTy(m_context);
}

llvm::Type* Codegen::generate([[maybe_unused]] const types::Bool& type) {
    return llvm::Type::getInt1Ty(m_context);
}

llvm::Type* Codegen::generate([[maybe_unused]] const types::Null& type) {
    return nullptr;
}

llvm::Type* Codegen::generate([[maybe_unused]] const types::Undefined& type) {
    return nullptr;
}

llvm::Type* Codegen::generate([[maybe_unused]] const types::Void& type) {
    return llvm::Type::getVoidTy(m_context);
}

llvm::Type* Codegen::generate(const types::Pointer& type) {
    auto* llvm_type = type.type->codegen(*this);

    return llvm_type->getPointerTo();
}

llvm::Type* Codegen::generate(const types::Struct& type) { return type.type; }

llvm::Type* Codegen::generate(const types::Union& type) { return type.type; }

llvm::Type* Codegen::generate(const types::Enum& type) {
    return type.type->codegen(*this);
}

llvm::Type* Codegen::generate(const types::Alias& type) {
    return type.type->codegen(*this);
}

llvm::Type* Codegen::generate(const types::Optional& type) {
    auto* contained = type.type->codegen(*this);

    if (is<types::Pointer>(*type.type)) {
        return contained;
    }

    auto iterator = m_optional_types.find(contained);

    if (iterator != m_optional_types.end()) {
        return iterator->second;
    }

    auto* llvm_type =
        llvm::StructType::create({contained, llvm::Type::getInt1Ty(m_context)});

    m_optional_types[contained] = llvm_type;

    return llvm_type;
}

llvm::Type* Codegen::generate(const types::Range& type) {
    auto* contained = type.type->codegen(*this);

    auto iterator = m_range_types.find(contained);

    if (iterator != m_range_types.end()) {
        return iterator->second;
    }

    auto* llvm_type = llvm::StructType::create({contained, contained});

    m_range_types[contained] = llvm_type;

    return llvm_type;
}

llvm::Type* Codegen::generate(const types::Array& type) {
    auto* llvm_type = type.type->codegen(*this);

    return llvm::ArrayType::get(llvm_type, type.size);
}

llvm::Type* Codegen::generate(const types::Slice& type) { return m_slice_type; }

llvm::Type* Codegen::generate(const types::Tuple& type) { return type.type; }

llvm::Type* Codegen::generate(const types::Function& type) {
    std::vector<llvm::Type*> llvm_param_types;
    llvm_param_types.reserve(type.param_types.size());

    for (const auto& parameter : type.param_types) {
        llvm_param_types.push_back(parameter->codegen(*this));
    }

    return llvm::FunctionType::get(
        type.return_type->codegen(*this), llvm_param_types, type.variadic);
}

bool Codegen::types_equal(const Type& lhs, const Type& rhs) const {
    if (&lhs == &rhs) {
        return true;
    }

    if (const auto* lhs_pointer = dyn_cast<types::Pointer>(lhs)) {
        if (const auto* rhs_pointer = dyn_cast<types::Pointer>(rhs)) {
            return lhs_pointer->is_mutable == rhs_pointer->is_mutable &&
                   types_equal(*lhs_pointer->type, *rhs_pointer->type);
        }
    }

    if (const auto* lhs_optional = dyn_cast<types::Optional>(lhs)) {
        if (const auto* rhs_optional = dyn_cast<types::Optional>(rhs)) {
            return types_equal(*lhs_optional->type, *rhs_optional->type);
        }
    }

    if (const auto* lhs_range = dyn_cast<types::Range>(lhs)) {
        if (const auto* rhs_range = dyn_cast<types::Range>(rhs)) {
            return types_equal(*lhs_range->type, *rhs_range->type);
        }
    }

    if (const auto* lhs_array = dyn_cast<types::Array>(lhs)) {
        if (const auto* rhs_array = dyn_cast<types::Array>(rhs)) {
            return lhs_array->size == rhs_array->size &&
                   types_equal(*lhs_array->type, *rhs_array->type);
        }
    }

    if (const auto* lhs_slice = dyn_cast<types::Slice>(lhs)) {
        if (const auto* rhs_slice = dyn_cast<types::Slice>(rhs)) {
            return types_equal(*lhs_slice->type, *rhs_slice->type);
        }
    }

    if (const auto* lhs_func = dyn_cast<types::Function>(lhs)) {
        const auto* rhs_func = dyn_cast<types::Function>(rhs);

        if (!rhs_func) {
            return false;
        }

        if (lhs_func->variadic != rhs_func->variadic) {
            return false;
        }

        if (lhs_func->param_types.size() != rhs_func->param_types.size()) {
            return false;
        }

        if (!types_equal(*lhs_func->return_type, *rhs_func->return_type)) {
            return false;
        }

        for (std::size_t i = 0; i < lhs_func->param_types.size(); ++i) {
            if (!types_equal(
                    *lhs_func->param_types[i], *rhs_func->param_types[i])) {
                return false;
            }
        }

        return true;
    }

    return false;
}

std::shared_ptr<types::Function>
Codegen::generate_fn_type(const ast::FnProto& proto) {
    std::shared_ptr<Type> return_type = m_void_type;

    if (proto.return_type) {
        return_type = proto.return_type->codegen(*this);

        if (!return_type) {
            return nullptr;
        }
    }

    std::vector<std::shared_ptr<Type>> param_types;
    std::vector<llvm::Constant*> default_args;

    param_types.reserve(proto.params.size());

    for (const auto& parameter : proto.params) {
        auto type = parameter.type->codegen(*this);

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
            type_mismatch(parameter.value->offset, *type, *value->type);
            return nullptr;
        }

        if (!llvm::isa<llvm::Constant>(val->value)) {
            error(parameter.value->offset, "not a constant");
            return nullptr;
        }

        default_args.push_back(static_cast<llvm::Constant*>(val->value));
    }

    return std::make_shared<types::Function>(
        return_type, std::move(param_types), std::move(default_args),
        proto.variadic);
}

} // namespace cent::backend
