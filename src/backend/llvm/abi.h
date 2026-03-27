#ifndef CENT_BACKEND_ABI_H
#define CENT_BACKEND_ABI_H

#include <cstddef>

#include <llvm/IR/DataLayout.h>
#include <llvm/TargetParser/Triple.h>

#include "backend/llvm/type.h"

namespace cent::backend {

namespace sysv {

// https://refspecs.linuxbase.org/elf/x86_64-abi-0.99.pdf

enum struct RegClass {
    Pointer,
    Integer,
    Sse,
    SseUp,
    X87,
    X87Up,
    ComplexX87,
    NoClass,
    Memory
};

struct Classification {
    RegClass first;
    RegClass second;
};

namespace detail {

[[nodiscard]] inline RegClass merge(RegClass first, RegClass second) {
    using enum RegClass;

    if (first == second) {
        return first;
    }

    if (first == NoClass) {
        return second;
    }

    if (second == NoClass) {
        return first;
    }

    if (first == Memory || second == Memory) {
        return Memory;
    }

    if (first == Pointer && second == NoClass) {
        return Pointer;
    }

    if (first == Integer || second == Integer || first == Pointer ||
        second == Pointer) {
        return Integer;
    }

    if (first == Sse && second == Sse) {
        return Sse;
    }

    return Memory;
}

[[nodiscard]] inline Classification
classify(RegClass reg, std::size_t offset, std::size_t size) {
    Classification result{RegClass::NoClass, RegClass::NoClass};

    std::size_t low = offset / 8;
    std::size_t high = (offset + size - 1) / 8;

    if (low == 0) {
        result.first = merge(result.first, reg);
    } else {
        result.second = merge(result.second, reg);
    }

    if (low == high) {
        return result;
    }

    if (high == 0) {
        result.first = merge(result.first, reg);
    } else {
        result.second = merge(result.second, reg);
    }

    return result;
}

[[nodiscard]] inline Classification
merge(Classification first, Classification second) {
    return {
        merge(first.first, second.first), merge(first.second, second.second)};
}

[[nodiscard]] inline Classification
classify(llvm::Type* type, std::size_t offset, const llvm::DataLayout& layout) {
    if (type->isIntegerTy()) {
        return classify(
            RegClass::Integer, offset, layout.getTypeAllocSize(type));
    }

    if (type->isPointerTy()) {
        return classify(
            RegClass::Pointer, offset, layout.getTypeAllocSize(type));
    }

    if (type->isFloatingPointTy()) {
        return classify(RegClass::Sse, offset, layout.getTypeAllocSize(type));
    }

    if (auto* struct_type = llvm::dyn_cast<llvm::StructType>(type)) {
        const auto& struct_layout = layout.getStructLayout(struct_type);

        Classification result{RegClass::NoClass, RegClass::NoClass};

        for (std::size_t i = 0; i < struct_type->getNumElements(); ++i) {
            auto classification = classify(
                struct_type->getElementType(i),
                offset + struct_layout->getElementOffset(i).getFixedValue(),
                layout);

            result = merge(result, classification);
        }

        return result;
    }

    if (auto* array = llvm::dyn_cast<llvm::ArrayType>(type)) {
        Classification result{RegClass::NoClass, RegClass::NoClass};
        std::size_t element = layout.getTypeAllocSize(array->getElementType());

        for (std::size_t i = 0; i < array->getNumElements(); ++i) {
            auto classification =
                classify(array->getElementType(), offset + i * element, layout);

            result = merge(result, classification);
        }

        return result;
    }

    return {RegClass::Memory, RegClass::Memory};
}

[[nodiscard]] inline llvm::Type*
lower_eightbyte(RegClass reg, std::size_t bitsize, llvm::LLVMContext& context) {
    if (reg == RegClass::Pointer) {
        return llvm::PointerType::get(context, 0);
    }

    if (reg == RegClass::Integer) {
        return llvm::IntegerType::get(context, bitsize);
    }

    if (reg == RegClass::Sse) {
        if (bitsize == 32) {
            return llvm::Type::getFloatTy(context);
        }

        if (bitsize == 64) {
            return llvm::Type::getDoubleTy(context);
        }
    }

    return nullptr;
}

} // namespace detail

[[nodiscard]] inline Classification
classify(llvm::Type* type, const llvm::DataLayout& layout) {
    using enum RegClass;

    if (layout.getTypeAllocSize(type) > 16) {
        return {Memory, Memory};
    }

    auto result = detail::classify(type, 0, layout);

    if (result.first == Memory || result.second == Memory) {
        return {Memory, Memory};
    }

    if (result.first == NoClass) {
        result.first = Integer;
    }

    if (result.second == SseUp && result.first != Sse) {
        result.second = Sse;
    }

    if ((result.first == Integer && result.second == Sse) ||
        (result.first == Sse && result.second == Integer)) {
        return {Memory, Memory};
    }

    return result;
}

[[nodiscard]] inline bool
should_sret(llvm::Type* type, const llvm::DataLayout& layout) {
    if (type->isVoidTy()) {
        return false;
    }

    if (layout.getTypeAllocSize(type) > 16) {
        return true;
    }

    auto classification = classify(type, layout);
    return classification.first == RegClass::Memory;
}

[[nodiscard]] inline llvm::Type* lower(
    Type* type, const Classification& classification,
    const llvm::DataLayout& layout, llvm::LLVMContext& context) {
    std::size_t bitsize = layout.getTypeAllocSizeInBits(type->llvm_type);

    if (classification.first == RegClass::Memory) {
        return llvm::PointerType::getUnqual(type->llvm_type);
    }

    if (classification.second == RegClass::NoClass) {
        return detail::lower_eightbyte(classification.first, bitsize, context);
    }

    return llvm::StructType::get(
        context, {detail::lower_eightbyte(classification.first, 64, context),
                  detail::lower_eightbyte(
                      classification.second, bitsize - 64, context)});
}

} // namespace sysv

enum struct Abi { SysV, Unknown };

[[nodiscard]] inline Abi get_abi(const llvm::Triple& triple) {
    switch (triple.getArch()) {
    case llvm::Triple::x86_64:
        if (triple.isOSLinux()) {
            return Abi::SysV;
        }

        break;
    default:
        break;
    }

    return Abi::Unknown;
}

} // namespace cent::backend

#endif
