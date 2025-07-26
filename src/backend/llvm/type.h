#ifndef CENT_BACKEND_TYPE_H
#define CENT_BACKEND_TYPE_H

#include <cstdint>
#include <string>

#include <llvm/IR/Type.h>

namespace cent::backend {

struct Type {
    enum struct Kind : std::uint8_t {
        I8,
        I16,
        I32,
        I64,
        ISize,
        U8,
        U16,
        U32,
        U64,
        USize,
        F32,
        F64,
        Bool,
        Rune,
        Null,
        Undefined,
        Void,
        Never,

        Pointer,
        Optional,
        Range,
        Array,
        Slice,
        Tuple,

        Enum,
        Struct,
        Union,
        Function,
        Alias,

        TemplateParam,
        TemplateStructInst,
        TemplateUnionInst
    };

    [[nodiscard]] Type(Kind kind, llvm::Type* llvm_type)
    : kind{kind}, llvm_type{llvm_type} {};

    virtual ~Type() = default;

    Type(const Type&) = delete;
    Type(Type&&) = delete;

    auto operator=(const Type&) = delete;
    auto operator=(Type&&) = delete;

    [[nodiscard]] virtual std::string to_string() const = 0;

    const Kind kind;
    llvm::Type* const llvm_type;
};

template <typename Derived> [[nodiscard]] inline bool is(const Type* value) {
    return Derived::class_of(value);
}

template <typename DerivedFirst, typename DerivedSecond, typename... Derived>
[[nodiscard]] inline bool is(const Type* value) {
    return DerivedFirst::class_of(value) ||
           is<DerivedSecond, Derived...>(value);
}

template <typename Derived>
[[nodiscard]] inline const Derived* dyn_cast(const Type* value) {
    if (Derived::class_of(value)) {
        return static_cast<const Derived*>(value);
    }

    return nullptr;
}

template <typename Derived>
[[nodiscard]] inline Derived* dyn_cast(Type* value) {
    if (Derived::class_of(value)) {
        return static_cast<Derived*>(value);
    }

    return nullptr;
}

namespace detail {

template <typename Derived, Type::Kind DerivedKind> struct Ty : Type {
    [[nodiscard]] Ty(llvm::Type* llvm_type) : Type{DerivedKind, llvm_type} {};

    [[nodiscard]] static bool class_of(const Type* type) {
        return type->kind == DerivedKind;
    }
};

} // namespace detail

} // namespace cent::backend

#endif
