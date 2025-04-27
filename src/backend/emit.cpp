#include <fmt/core.h>

#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Support/FileSystem.h>

#include <llvm/Analysis/CGSCCPassManager.h>
#include <llvm/Analysis/LoopAnalysisManager.h>

#include "cent/log.h"

#include "cent/backend/emit.h"

namespace cent::backend {

void optimize_module(
    llvm::Module& module, llvm::OptimizationLevel opt_level) noexcept {
    llvm::LoopAnalysisManager loop_manager;
    llvm::FunctionAnalysisManager function_manager;
    llvm::CGSCCAnalysisManager cg_manager;
    llvm::ModuleAnalysisManager module_manager;

    llvm::PassBuilder builder;

    builder.registerModuleAnalyses(module_manager);
    builder.registerCGSCCAnalyses(cg_manager);
    builder.registerFunctionAnalyses(function_manager);
    builder.registerLoopAnalyses(loop_manager);

    builder.crossRegisterProxies(
        loop_manager, function_manager, cg_manager, module_manager);

    auto manager = builder.buildPerModuleDefaultPipeline(opt_level);
    manager.run(module, module_manager);
}

bool emit_obj(
    llvm::Module& module, llvm::TargetMachine& machine,
    const std::filesystem::path& path) noexcept {
    std::error_code code;
    llvm::raw_fd_ostream file{path.string(), code, llvm::sys::fs::OF_None};

    if (code) {
        error(fmt::format("could not open file: {}", code.message()));

        return false;
    }

    llvm::legacy::PassManager manager;

    machine.addPassesToEmitFile(manager, file, nullptr, llvm::CGFT_ObjectFile);

    manager.run(module);
    file.flush();

    return true;
}

} // namespace cent::backend
