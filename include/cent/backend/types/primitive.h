#ifndef CENT_BACKEND_TYPES_PRIMITIVE_H
#define CENT_BACKEND_TYPES_PRIMITIVE_H

#include <cstddef>
#include <memory>
#include <utility>
#include <vector>

#include <fmt/core.h>

#include "cent/backend/type.h"

namespace cent::backend::types {

struct I8 : detail::Type<I8> {
    std::string to_string() noexcept override { return "i8"; }

    bool is_signed_int() noexcept override { return true; };
};

struct I16 : detail::Type<I16> {
    std::string to_string() noexcept override { return "i16"; }

    bool is_signed_int() noexcept override { return true; };
};

struct I32 : detail::Type<I32> {
    std::string to_string() noexcept override { return "i32"; }

    bool is_signed_int() noexcept override { return true; };
};

struct I64 : detail::Type<I64> {
    std::string to_string() noexcept override { return "i64"; }

    bool is_signed_int() noexcept override { return true; };
};

struct ISize : detail::Type<ISize> {
    std::string to_string() noexcept override { return "isize"; }

    bool is_signed_int() noexcept override { return true; };
};

struct U8 : detail::Type<U8> {
    std::string to_string() noexcept override { return "u8"; }

    bool is_unsigned_int() noexcept override { return true; };
};

struct U16 : detail::Type<U16> {
    std::string to_string() noexcept override { return "u16"; }

    bool is_unsigned_int() noexcept override { return true; };
};

struct U32 : detail::Type<U32> {
    std::string to_string() noexcept override { return "u32"; }

    bool is_unsigned_int() noexcept override { return true; };
};

struct U64 : detail::Type<U64> {
    std::string to_string() noexcept override { return "u64"; }

    bool is_unsigned_int() noexcept override { return true; };
};

struct USize : detail::Type<USize> {
    std::string to_string() noexcept override { return "usize"; }

    bool is_unsigned_int() noexcept override { return true; };
};

struct F32 : detail::Type<F32> {
    std::string to_string() noexcept override { return "f32"; }

    bool is_float() noexcept override { return true; };
};

struct F64 : detail::Type<F64> {
    std::string to_string() noexcept override { return "f64"; }

    bool is_float() noexcept override { return true; };
};

struct Str : detail::Type<Str> {
    std::string to_string() noexcept override { return "str"; }
};

struct Bool : detail::Type<Bool> {
    std::string to_string() noexcept override { return "bool"; }

    bool is_bool() noexcept override { return true; };
};

struct Null : detail::Type<Null> {
    std::string to_string() noexcept override { return "null"; }

    bool is_null() noexcept override { return true; };
};

struct Void : detail::Type<Void> {
    std::string to_string() noexcept override { return "void"; }
};

struct Pointer : detail::Type<Pointer> {
    [[nodiscard]] Pointer(
        std::shared_ptr<backend::Type> type, bool is_mutable) noexcept
    : type{std::move(type)}, is_mutable{is_mutable} {}

    std::string to_string() noexcept override {
        return (is_mutable ? "*mut " : "*") + type->to_string();
    }

    bool is_pointer() noexcept override { return true; };

    std::shared_ptr<backend::Type> type;
    bool is_mutable;
};

struct Optional : detail::Type<Optional> {
    [[nodiscard]] Optional(std::shared_ptr<backend::Type> type) noexcept
    : type{std::move(type)} {}

    std::string to_string() noexcept override {
        return "?" + type->to_string();
    }

    bool is_optional() noexcept override { return true; };

    std::shared_ptr<backend::Type> type;
};

struct Array : detail::Type<Array> {
    [[nodiscard]] Array(
        std::shared_ptr<backend::Type> type, std::size_t size) noexcept
    : type{std::move(type)}, size{size} {}

    std::string to_string() noexcept override {
        return fmt::format("[{}, {}]", type->to_string(), size);
    }

    bool is_array() noexcept override { return true; };

    std::shared_ptr<backend::Type> type;
    std::size_t size;
};

struct Tuple : detail::Type<Tuple> {
    [[nodiscard]] Tuple(
        llvm::StructType* type,
        std::vector<std::shared_ptr<backend::Type>> types) noexcept
    : type{type}, types{std::move(types)} {}

    std::string to_string() noexcept override {
        std::string result = "(";

        for (std::size_t i = 0; i < types.size(); ++i) {
            result += types[i]->to_string();

            if (i + 1 != types.size()) {
                result += ", ";
            }
        }

        return result + ")";
    }

    bool is_tuple() noexcept override { return true; };

    llvm::StructType* type;
    std::vector<std::shared_ptr<backend::Type>> types;
};

} // namespace cent::backend::types

#endif
