#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <span>
#include <string>
#include <vector>

#include <fmt/core.h>

#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/TargetParser/Host.h>

#include "cent/frontend/parser.h"

#include "cent/backend/llvm/codegen.h"
#include "cent/backend/llvm/emit.h"

#include "cent/log.h"
#include "cent/util.h"

void help() { fmt::print("Usage: centc FILE...\n"); }

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
        cent::log::error(message);

        return 1;
    }

    auto* machine = target->createTargetMachine(
        triple, "generic", "", llvm::TargetOptions{}, llvm::Reloc::PIC_);

    std::vector<std::filesystem::path> source_files;
    source_files.reserve(args.size());

    bool compile_only = false;
    bool optimize = false;
    bool emit_llvm = false;

    for (const char* arg_cstr : args.subspan(1)) {
        std::string arg = arg_cstr;

        if (arg == "-c") {
            compile_only = true;
            continue;
        }

        if (arg == "-O") {
            optimize = true;
            continue;
        }

        if (arg == "--emit-llvm") {
            emit_llvm = true;
            continue;
        }

        source_files.emplace_back(arg);
    }

    std::vector<std::filesystem::path> object_files;
    object_files.reserve(args.size());

    for (auto& file : source_files) {
        auto code = cent::read_file(file);

        if (!code) {
            return 1;
        }

        std::string filename = file.string();

        cent::frontend::Parser parser{*code, filename};
        auto program = parser.parse();

        if (!program) {
            return 1;
        }

        cent::backend::Codegen codegen{
            std::move(program), *code, filename, machine->createDataLayout(),
            triple};

        auto module = codegen.generate();

        if (!module) {
            return 1;
        }

        if (optimize) {
            cent::backend::optimize_module(
                *module, llvm::OptimizationLevel::O3);
        }

        if (emit_llvm) {
            std::filesystem::path ir_file = file;
            ir_file.replace_extension(".ll");

            if (!cent::backend::emit_llvm(*module, ir_file)) {
                return 1;
            }

            continue;
        }

        std::filesystem::path object_file =
            compile_only ? file : std::tmpnam(nullptr);

        object_file.replace_extension(".o");

        if (!cent::backend::emit_obj(*module, *machine, object_file)) {
            return 1;
        }

        object_files.push_back(object_file);
    }

    if (!compile_only && !emit_llvm) {
        std::string command = "gcc -o main";

        for (auto& file : object_files) {
            command += ' ';
            command += file;
        }

        auto exit_code = std::system(command.c_str());

        for (auto& file : object_files) {
            std::filesystem::remove(file);
        }

        return exit_code;
    }

    return 0;
}
