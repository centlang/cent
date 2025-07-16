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

enum struct EmitType : std::uint8_t { Obj, Exe, LlvmIr, LlvmBc };

void help() {
    fmt::print(R"(USAGE: centc [options] file [-- linker options]

OPTIONS:
  -O                    Enable optimizations
  -o <file>             Place the output into <file>
  --emit <type>         Specify the compiler output type
  --target <triple>     Specify the target triple
  --help                Print this help message and exit
)");
}

int main(int argc, char** argv) {
    std::span args{argv, static_cast<std::size_t>(argc)};

    if (args.size() < 2) {
        help();
        return 0;
    }

    std::string target_triple = llvm::sys::getDefaultTargetTriple();

    std::optional<std::filesystem::path> source_file = std::nullopt;
    std::optional<std::filesystem::path> output_file = std::nullopt;

    std::vector<std::string> linker_options;

    bool optimize = false;
    EmitType emit_type = EmitType::Exe;

    bool expecting_output = false;
    bool expecting_target = false;
    bool expecting_emit_type = false;
    bool parsing_linker_options = false;

    for (const char* arg_cstr : args.subspan(1)) {
        std::string arg = arg_cstr;

        if (parsing_linker_options) {
            linker_options.push_back(std::move(arg));
            continue;
        }

        if (expecting_output) {
            output_file = arg;
            expecting_output = false;
            continue;
        }

        if (expecting_target) {
            target_triple = arg;
            expecting_target = false;
            continue;
        }

        if (expecting_emit_type) {
            expecting_emit_type = false;

            if (arg == "obj") {
                emit_type = EmitType::Obj;
                continue;
            }

            if (arg == "exe") {
                emit_type = EmitType::Exe;
                continue;
            }

            if (arg == "llvm-ir") {
                emit_type = EmitType::LlvmIr;
                continue;
            }

            if (arg == "llvm-bc") {
                emit_type = EmitType::LlvmBc;
                continue;
            }

            cent::log::error(fmt::format(
                "unrecognized emit type: {}", cent::log::quoted(arg)));

            continue;
        }

        if (arg == "--help") {
            help();
            return 0;
        }

        if (arg == "-O") {
            optimize = true;
            continue;
        }

        if (arg == "--emit") {
            expecting_emit_type = true;
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

        if (arg == "--") {
            parsing_linker_options = true;
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

    if (expecting_emit_type) {
        cent::log::error("missing emit type");
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

    const auto* target =
        llvm::TargetRegistry::lookupTarget(target_triple, message);

    if (!target) {
        cent::log::error(message);
        return 1;
    }

    auto* machine = target->createTargetMachine(
        target_triple, "generic", "", llvm::TargetOptions{}, llvm::Reloc::PIC_);

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
        std::move(program), filename, machine->createDataLayout(),
        target_triple};

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
        if (output_file) {
            return *output_file;
        }

        auto result = *source_file;
        result.replace_extension(extension);

        return result;
    };

    switch (emit_type) {
    case EmitType::LlvmIr:
        if (!cent::backend::emit_llvm_ir(*module, get_output_file(".ll"))) {
            return 1;
        }

        break;
    case EmitType::LlvmBc:
        if (!cent::backend::emit_llvm_bc(*module, get_output_file(".bc"))) {
            return 1;
        }

        break;
    case EmitType::Obj:
        if (!cent::backend::emit_obj(
                *module, *machine, get_output_file(".o"))) {
            return 1;
        }

        break;
    case EmitType::Exe:
        std::filesystem::path object_file = std::tmpnam(nullptr);

        if (!cent::backend::emit_obj(*module, *machine, object_file)) {
            return 1;
        }

        std::vector<std::string> args = {
            "-o", get_output_file(""), object_file};

        args.insert(args.end(), linker_options.begin(), linker_options.end());

        int exit_code = cent::exec_command("gcc", args);

        std::filesystem::remove(object_file);

        return exit_code;
    }

    return 0;
}
