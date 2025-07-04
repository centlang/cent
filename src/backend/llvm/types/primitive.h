#ifndef CENT_BACKEND_TYPES_PRIMITIVE_H
#define CENT_BACKEND_TYPES_PRIMITIVE_H

#include <cstddef>
#include <utility>
#include <vector>

#include <fmt/core.h>

#include "backend/llvm/type.h"

namespace cent::backend::types {

struct I8 : detail::Ty<I8, Type::Kind::I8> {
    [[nodiscard]] I8(llvm::Type* llvm_type) : Ty{llvm_type} {}

    [[nodiscard]] std::string to_string() const override { return "i8"; }
};

struct I16 : detail::Ty<I16, Type::Kind::I16> {
    [[nodiscard]] I16(llvm::Type* llvm_type) : Ty{llvm_type} {}

    [[nodiscard]] std::string to_string() const override { return "i16"; }
};

struct I32 : detail::Ty<I32, Type::Kind::I32> {
    [[nodiscard]] I32(llvm::Type* llvm_type) : Ty{llvm_type} {}

    [[nodiscard]] std::string to_string() const override { return "i32"; }
};

struct I64 : detail::Ty<I64, Type::Kind::I64> {
    [[nodiscard]] I64(llvm::Type* llvm_type) : Ty{llvm_type} {}

    [[nodiscard]] std::string to_string() const override { return "i64"; }
};

struct ISize : detail::Ty<ISize, Type::Kind::ISize> {
    [[nodiscard]] ISize(llvm::Type* llvm_type) : Ty{llvm_type} {}

    [[nodiscard]] std::string to_string() const override { return "isize"; }
};

struct U8 : detail::Ty<U8, Type::Kind::U8> {
    [[nodiscard]] U8(llvm::Type* llvm_type) : Ty{llvm_type} {}

    [[nodiscard]] std::string to_string() const override { return "u8"; }
};

struct U16 : detail::Ty<U16, Type::Kind::U16> {
    [[nodiscard]] U16(llvm::Type* llvm_type) : Ty{llvm_type} {}

    [[nodiscard]] std::string to_string() const override { return "u16"; }
};

struct U32 : detail::Ty<U32, Type::Kind::U32> {
    [[nodiscard]] U32(llvm::Type* llvm_type) : Ty{llvm_type} {}

    [[nodiscard]] std::string to_string() const override { return "u32"; }
};

struct U64 : detail::Ty<U64, Type::Kind::U64> {
    [[nodiscard]] U64(llvm::Type* llvm_type) : Ty{llvm_type} {}

    [[nodiscard]] std::string to_string() const override { return "u64"; }
};

struct USize : detail::Ty<USize, Type::Kind::USize> {
    [[nodiscard]] USize(llvm::Type* llvm_type) : Ty{llvm_type} {}

    [[nodiscard]] std::string to_string() const override { return "usize"; }
};

struct F32 : detail::Ty<F32, Type::Kind::F32> {
    [[nodiscard]] F32(llvm::Type* llvm_type) : Ty{llvm_type} {}

    [[nodiscard]] std::string to_string() const override { return "f32"; }
};

struct F64 : detail::Ty<F64, Type::Kind::F64> {
    [[nodiscard]] F64(llvm::Type* llvm_type) : Ty{llvm_type} {}

    [[nodiscard]] std::string to_string() const override { return "f64"; }
};

struct Bool : detail::Ty<Bool, Type::Kind::Bool> {
    [[nodiscard]] Bool(llvm::Type* llvm_type) : Ty{llvm_type} {}

    [[nodiscard]] std::string to_string() const override { return "bool"; }
};

struct Null : detail::Ty<Null, Type::Kind::Null> {
    [[nodiscard]] Null(llvm::Type* llvm_type) : Ty{llvm_type} {}

    [[nodiscard]] std::string to_string() const override { return "null"; }
};

struct Undefined : detail::Ty<Undefined, Type::Kind::Undefined> {
    [[nodiscard]] Undefined(llvm::Type* llvm_type) : Ty{llvm_type} {}

    [[nodiscard]] std::string to_string() const override { return "undefined"; }
};

struct Void : detail::Ty<Void, Type::Kind::Void> {
    [[nodiscard]] Void(llvm::Type* llvm_type) : Ty{llvm_type} {}

    [[nodiscard]] std::string to_string() const override { return "void"; }
};

struct Pointer : detail::Ty<Pointer, Type::Kind::Pointer> {
    [[nodiscard]] Pointer(llvm::Type* llvm_type, Type* type, bool is_mutable)
    : Ty{llvm_type}, type{type}, is_mutable{is_mutable} {}

    [[nodiscard]] std::string to_string() const override {
        return (is_mutable ? "*mut " : "*") + type->to_string();
    }

    Type* type;
    bool is_mutable;
};

struct Optional : detail::Ty<Optional, Type::Kind::Optional> {
    [[nodiscard]] Optional(llvm::Type* llvm_type, Type* type)
    : Ty{llvm_type}, type{type} {}

    [[nodiscard]] std::string to_string() const override {
        return "?" + type->to_string();
    }

    Type* type;
};

struct Range : detail::Ty<Range, Type::Kind::Range> {
    [[nodiscard]] Range(llvm::Type* llvm_type, Type* type)
    : Ty{llvm_type}, type{type} {}

    [[nodiscard]] std::string to_string() const override {
        return ".." + type->to_string();
    }

    Type* type;
};

struct Array : detail::Ty<Array, Type::Kind::Array> {
    [[nodiscard]] Array(llvm::Type* llvm_type, Type* type, std::size_t size)
    : Ty{llvm_type}, type{type}, size{size} {}

    [[nodiscard]] std::string to_string() const override {
        return fmt::format("[{}]{}", size, type->to_string());
    }

    Type* type;
    std::size_t size;
};

struct Slice : detail::Ty<Slice, Type::Kind::Slice> {
    [[nodiscard]] Slice(llvm::Type* llvm_type, Type* type, bool is_mutable)
    : Ty{llvm_type}, type{type}, is_mutable{is_mutable} {}

    [[nodiscard]] std::string to_string() const override {
        return fmt::format(
            "[]{}{}", is_mutable ? "mut " : "", type->to_string());
    }

    Type* type;
    bool is_mutable;
};

struct Tuple : detail::Ty<Tuple, Type::Kind::Tuple> {
    [[nodiscard]] Tuple(llvm::Type* llvm_type, std::vector<Type*> types)
    : Ty{llvm_type}, types{std::move(types)} {}

    [[nodiscard]] std::string to_string() const override {
        std::string result = "(";

        for (std::size_t i = 0; i < types.size(); ++i) {
            result += types[i]->to_string();

            if (i + 1 != types.size()) {
                result += ", ";
            }
        }

        return result + ")";
    }

    std::vector<Type*> types;
};

} // namespace cent::backend::types

#endif
