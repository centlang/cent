#include <fmt/core.h>

#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Support/FileSystem.h>

#include "cent/log.h"

#include "cent/backend/emit.h"

namespace cent::backend {

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
