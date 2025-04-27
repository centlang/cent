#include <cstdio>
#include <cstring>
#include <fstream>
#include <span>
#include <sstream>

#include <fmt/core.h>

#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/TargetParser/Host.h>

#include "cent/backend/codegen.h"
#include "cent/backend/emit.h"
#include "cent/frontend/parser.h"

#include "cent/log.h"

void help() noexcept { fmt::print("Usage: centc FILE\n"); }

int main(int argc, char** argv) {
    std::span args{argv, static_cast<std::size_t>(argc)};

    if (args.size() != 2) {
        help();

        return 0;
    }

    std::string filename = args[1];
    std::ifstream file{filename};

    if (!file) {
        cent::error(
            fmt::format("could not open file: {}", std::strerror(errno)));

        return 1;
    }

    std::ostringstream buffer;
    buffer << file.rdbuf();

    std::string code = buffer.str();

    cent::frontend::Parser parser{code, filename};
    auto program = parser.parse();

    if (!program) {
        return 1;
    }

    cent::backend::Codegen codegen{std::move(program), filename};
    auto module = codegen.generate();

    if (!module) {
        return 1;
    }

    auto triple = llvm::sys::getDefaultTargetTriple();

    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    std::string message;

    const auto* target = llvm::TargetRegistry::lookupTarget(triple, message);

    if (!target) {
        cent::error(message);

        return 1;
    }

    auto* machine = target->createTargetMachine(
        triple, "generic", "", llvm::TargetOptions{}, llvm::Reloc::PIC_);

    module->setDataLayout(machine->createDataLayout());
    module->setTargetTriple(triple);

    cent::backend::optimize_module(*module, llvm::OptimizationLevel::O3);

    if (!cent::backend::emit_obj(
            *module, *machine,
            filename.substr(0, filename.find_last_of('.')) + ".o")) {
        return 1;
    }

    return 0;
}
