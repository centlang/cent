#ifndef CENT_BACKEND_EMIT_H
#define CENT_BACKEND_EMIT_H

#include <filesystem>

#include <llvm/IR/Module.h>
#include <llvm/Target/TargetMachine.h>

namespace cent::backend {

[[nodiscard]] bool emit_obj(
    llvm::Module& module, llvm::TargetMachine& machine,
    const std::filesystem::path& path) noexcept;

} // namespace cent::backend

#endif
