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

void help() { fmt::print("Usage: centc [options] FILE...\n"); }

int main(int argc, char** argv) {
    std::span args{argv, static_cast<std::size_t>(argc)};

    if (args.size() < 2) {
        help();

        return 0;
    }

    std::string triple = llvm::sys::getDefaultTargetTriple();

    std::vector<std::filesystem::path> source_files;
    source_files.reserve(args.size());

    std::optional<std::filesystem::path> output = std::nullopt;

    bool compile_only = false;
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

        if (arg == "-c") {
            compile_only = true;
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

        source_files.emplace_back(arg);
    }

    if (expecting_output) {
        cent::log::error("missing filename");
        return 1;
    }

    if (expecting_target) {
        cent::log::error("missing target triple");
        return 1;
    }

    if (output && source_files.size() > 1 &&
        (compile_only || emit_llvm_ir || emit_llvm_bc)) {
        cent::log::error("cannot specify `-o` with multiple output files");
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

    std::vector<std::filesystem::path> object_files;
    object_files.reserve(args.size());

    bool had_error = false;

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
            std::move(program), filename, machine->createDataLayout(), triple};

        auto module = codegen.generate();

        if (!module) {
            return 1;
        }

        if (parser.had_error() || codegen.had_error()) {
            had_error = true;
            continue;
        }

        if (optimize) {
            cent::backend::optimize_module(
                *module, llvm::OptimizationLevel::O3);
        }

        auto get_output_file = [&](std::string_view extension) {
            if (output) {
                return *output;
            }

            auto result = file;
            result.replace_extension(extension);

            return result;
        };

        if (emit_llvm_ir) {
            if (!cent::backend::emit_llvm_ir(*module, get_output_file(".ll"))) {
                return 1;
            }

            continue;
        }

        if (emit_llvm_bc) {
            if (!cent::backend::emit_llvm_bc(*module, get_output_file(".bc"))) {
                return 1;
            }

            continue;
        }

        std::filesystem::path object_file =
            compile_only ? get_output_file(".o") : std::tmpnam(nullptr);

        if (!cent::backend::emit_obj(*module, *machine, object_file)) {
            return 1;
        }

        object_files.push_back(object_file);
    }

    if (had_error) {
        return 1;
    }

    if (!compile_only && !emit_llvm_ir && !emit_llvm_bc) {
        std::string command = "gcc -o " + (output ? output->string() : "main");

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
