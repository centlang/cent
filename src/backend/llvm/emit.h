#ifndef CENT_BACKEND_EMIT_H
#define CENT_BACKEND_EMIT_H

#include <filesystem>

#include <llvm/IR/Module.h>
#include <llvm/Passes/OptimizationLevel.h>
#include <llvm/Target/TargetMachine.h>

namespace cent::backend {

void optimize_module(llvm::Module& module, llvm::OptimizationLevel opt_level);

[[nodiscard]] bool emit_obj(
    llvm::Module& module, llvm::TargetMachine& machine,
    const std::filesystem::path& path);

[[nodiscard]] bool
emit_llvm(llvm::Module& module, const std::filesystem::path& path);

} // namespace cent::backend

#endif
