#ifndef CENT_BACKEND_TYPES_PRIMITIVE_H
#define CENT_BACKEND_TYPES_PRIMITIVE_H

#include <cstddef>
#include <memory>
#include <utility>
#include <vector>

#include <fmt/core.h>

#include "backend/llvm/type.h"

namespace cent::backend::types {

struct I8 : detail::Type<I8, Type::Kind::I8> {
    std::string to_string() const override { return "i8"; }
};

struct I16 : detail::Type<I16, Type::Kind::I16> {
    std::string to_string() const override { return "i16"; }
};

struct I32 : detail::Type<I32, Type::Kind::I32> {
    std::string to_string() const override { return "i32"; }
};

struct I64 : detail::Type<I64, Type::Kind::I64> {
    std::string to_string() const override { return "i64"; }
};

struct ISize : detail::Type<ISize, Type::Kind::ISize> {
    std::string to_string() const override { return "isize"; }
};

struct U8 : detail::Type<U8, Type::Kind::U8> {
    std::string to_string() const override { return "u8"; }
};

struct U16 : detail::Type<U16, Type::Kind::U16> {
    std::string to_string() const override { return "u16"; }
};

struct U32 : detail::Type<U32, Type::Kind::U32> {
    std::string to_string() const override { return "u32"; }
};

struct U64 : detail::Type<U64, Type::Kind::U64> {
    std::string to_string() const override { return "u64"; }
};

struct USize : detail::Type<USize, Type::Kind::USize> {
    std::string to_string() const override { return "usize"; }
};

struct F32 : detail::Type<F32, Type::Kind::F32> {
    std::string to_string() const override { return "f32"; }
};

struct F64 : detail::Type<F64, Type::Kind::F64> {
    std::string to_string() const override { return "f64"; }
};

struct Bool : detail::Type<Bool, Type::Kind::Bool> {
    std::string to_string() const override { return "bool"; }
};

struct Null : detail::Type<Null, Type::Kind::Null> {
    std::string to_string() const override { return "null"; }
};

struct Undefined : detail::Type<Undefined, Type::Kind::Undefined> {
    std::string to_string() const override { return "undefined"; }
};

struct Void : detail::Type<Void, Type::Kind::Void> {
    std::string to_string() const override { return "void"; }
};

struct Pointer : detail::Type<Pointer, Type::Kind::Pointer> {
    [[nodiscard]] Pointer(std::shared_ptr<backend::Type> type, bool is_mutable)
    : type{std::move(type)}, is_mutable{is_mutable} {}

    std::string to_string() const override {
        return (is_mutable ? "*mut " : "*") + type->to_string();
    }

    std::shared_ptr<backend::Type> type;
    bool is_mutable;
};

struct Optional : detail::Type<Optional, Type::Kind::Optional> {
    [[nodiscard]] Optional(std::shared_ptr<backend::Type> type)
    : type{std::move(type)} {}

    std::string to_string() const override { return "?" + type->to_string(); }

    std::shared_ptr<backend::Type> type;
};

struct Range : detail::Type<Range, Type::Kind::Range> {
    [[nodiscard]] Range(std::shared_ptr<backend::Type> type)
    : type{std::move(type)} {}

    std::string to_string() const override { return ".." + type->to_string(); }

    std::shared_ptr<backend::Type> type;
};

struct Array : detail::Type<Array, Type::Kind::Array> {
    [[nodiscard]] Array(std::shared_ptr<backend::Type> type, std::size_t size)
    : type{std::move(type)}, size{size} {}

    std::string to_string() const override {
        return fmt::format("[{}]{}", size, type->to_string());
    }

    std::shared_ptr<backend::Type> type;
    std::size_t size;
};

struct Slice : detail::Type<Slice, Type::Kind::Slice> {
    [[nodiscard]] Slice(std::shared_ptr<backend::Type> type, bool is_mutable)
    : type{std::move(type)}, is_mutable{is_mutable} {}

    std::string to_string() const override {
        return fmt::format(
            "[]{}{}", is_mutable ? "mut " : "", type->to_string());
    }

    std::shared_ptr<backend::Type> type;
    bool is_mutable;
};

struct Tuple : detail::Type<Tuple, Type::Kind::Tuple> {
    [[nodiscard]] Tuple(
        llvm::StructType* type,
        std::vector<std::shared_ptr<backend::Type>> types)
    : type{type}, types{std::move(types)} {}

    std::string to_string() const override {
        std::string result = "(";

        for (std::size_t i = 0; i < types.size(); ++i) {
            result += types[i]->to_string();

            if (i + 1 != types.size()) {
                result += ", ";
            }
        }

        return result + ")";
    }

    llvm::StructType* type;
    std::vector<std::shared_ptr<backend::Type>> types;
};

} // namespace cent::backend::types

#endif
