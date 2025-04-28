#include <cstdio>
#include <cstring>
#include <span>

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
#include "cent/util.h"

void help() noexcept { fmt::print("Usage: centc FILE...\n"); }

int main(int argc, char** argv) {
    std::span args{argv, static_cast<std::size_t>(argc)};

    if (args.size() < 2) {
        help();

        return 0;
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

    for (const char* arg_cstr : args.subspan(1)) {
        std::string arg = arg_cstr;

        auto code = cent::read_file(arg);

        if (!code) {
            return 1;
        }

        cent::frontend::Parser parser{*code, arg};
        auto program = parser.parse();

        if (!program) {
            return 1;
        }

        cent::backend::Codegen codegen{std::move(program), arg};
        auto module = codegen.generate();

        if (!module) {
            return 1;
        }

        module->setDataLayout(machine->createDataLayout());
        module->setTargetTriple(triple);

        cent::backend::optimize_module(*module, llvm::OptimizationLevel::O3);

        if (!cent::backend::emit_obj(
                *module, *machine,
                arg.substr(0, arg.find_last_of('.')) + ".o")) {
            return 1;
        }
    }

    return 0;
}
