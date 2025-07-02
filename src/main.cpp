#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include <fmt/core.h>

#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/TargetParser/Host.h>

#include "frontend/parser.h"

#include "backend/llvm/codegen.h"
#include "backend/llvm/emit.h"

#include "log.h"
#include "util.h"

void help() { fmt::print("Usage: centc [options] FILE\n"); }

int main(int argc, char** argv) {
    std::span args{argv, static_cast<std::size_t>(argc)};

    if (args.size() < 2) {
        help();

        return 0;
    }

    std::string triple = llvm::sys::getDefaultTargetTriple();

    std::optional<std::filesystem::path> source_file;
    std::optional<std::filesystem::path> output = std::nullopt;

    bool optimize = false;
    bool emit_llvm_ir = false;
    bool emit_llvm_bc = false;

    bool expecting_output = false;
    bool expecting_target = false;

    for (const char* arg_cstr : args.subspan(1)) {
        std::string arg = arg_cstr;

        if (expecting_output) {
            output = arg;
            expecting_output = false;
            continue;
        }

        if (expecting_target) {
            triple = arg;
            expecting_target = false;
            continue;
        }

        if (arg == "-O") {
            optimize = true;
            continue;
        }

        if (arg == "--emit-llvm-ir") {
            emit_llvm_ir = true;
            continue;
        }

        if (arg == "--emit-llvm-bc") {
            emit_llvm_bc = true;
            continue;
        }

        if (arg == "-o") {
            expecting_output = true;
            continue;
        }

        if (arg == "--target") {
            expecting_target = true;
            continue;
        }

        if (arg.starts_with('-')) {
            cent::log::error(
                fmt::format("unrecognized option: {}", cent::log::quoted(arg)));

            return 1;
        }

        if (source_file) {
            cent::log::error("multiple input files provided");
            return 1;
        }

        source_file = arg;
    }

    if (expecting_output) {
        cent::log::error("missing filename");
        return 1;
    }

    if (expecting_target) {
        cent::log::error("missing target triple");
        return 1;
    }

    if (!source_file) {
        cent::log::error("no input file provided");
        return 1;
    }

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

    auto code = cent::read_file(*source_file);

    if (!code) {
        return 1;
    }

    std::string filename = source_file->string();

    cent::frontend::Parser parser{*code, filename};
    auto program = parser.parse();

    if (!program) {
        return 1;
    }

    cent::backend::Codegen codegen{
        std::move(program), filename, machine->createDataLayout(), triple};

    auto module = codegen.generate();

    if (!module) {
        return 1;
    }

    if (parser.had_error() || codegen.had_error()) {
        return 1;
    }

    if (optimize) {
        cent::backend::optimize_module(*module, llvm::OptimizationLevel::O3);
    }

    auto get_output_file = [&](std::string_view extension) {
        if (output) {
            return *output;
        }

        auto result = *source_file;
        result.replace_extension(extension);

        return result;
    };

    if (emit_llvm_ir) {
        if (!cent::backend::emit_llvm_ir(*module, get_output_file(".ll"))) {
            return 1;
        }

        return 0;
    }

    if (emit_llvm_bc) {
        if (!cent::backend::emit_llvm_bc(*module, get_output_file(".bc"))) {
            return 1;
        }

        return 0;
    }

    std::filesystem::path object_file = std::tmpnam(nullptr);

    if (!cent::backend::emit_obj(*module, *machine, object_file)) {
        return 1;
    }

    if (!emit_llvm_ir && !emit_llvm_bc) {
        std::vector<std::string> args = {
            "-o", get_output_file(""), object_file};

        int exit_code = cent::exec_command("gcc", args);

        std::filesystem::remove(object_file);
        return exit_code;
    }

    return 0;
}
