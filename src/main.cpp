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

struct Options {
    std::string target_triple;

    std::optional<std::filesystem::path> source_file = std::nullopt;
    std::optional<std::filesystem::path> output_file = std::nullopt;

    std::vector<std::string> linker_options;

    EmitType emit_type;

    bool optimize;
    bool run;
};

void version() { fmt::print("Cent v0.001\n"); }

void help() {
    fmt::print(R"(USAGE: centc [options] file [-- linker options]

OPTIONS:
  -O                    Enable optimizations
  -o <file>             Place the output into <file>
  --emit <type>         Specify the compiler output type
  --target <triple>     Specify the target triple
  --run                 Build and run immediately
  --help                Print this help message and exit
  --version             Print Cent version and exit
)");
}

[[nodiscard]] std::optional<Options> parse_args(std::span<char*> args) {
    if (args.size() < 2) {
        help();
        return std::nullopt;
    }

    Options options = {
        .target_triple = llvm::sys::getDefaultTargetTriple(),
        .emit_type = EmitType::Exe,
        .optimize = false};

    bool expecting_output = false;
    bool expecting_target = false;
    bool expecting_emit_type = false;
    bool parsing_linker_options = false;

    for (const char* arg_cstr : args.subspan(1)) {
        std::string arg = arg_cstr;

        if (parsing_linker_options) {
            options.linker_options.push_back(std::move(arg));
            continue;
        }

        if (expecting_output) {
            options.output_file = arg;
            expecting_output = false;
            continue;
        }

        if (expecting_target) {
            options.target_triple = arg;
            expecting_target = false;
            continue;
        }

        if (expecting_emit_type) {
            expecting_emit_type = false;

            if (arg == "obj") {
                options.emit_type = EmitType::Obj;
                continue;
            }

            if (arg == "exe") {
                options.emit_type = EmitType::Exe;
                continue;
            }

            if (arg == "llvm-ir") {
                options.emit_type = EmitType::LlvmIr;
                continue;
            }

            if (arg == "llvm-bc") {
                options.emit_type = EmitType::LlvmBc;
                continue;
            }

            cent::log::error(fmt::format(
                "unrecognized emit type: {}", cent::log::quoted(arg)));

            return std::nullopt;
        }

        if (arg == "--help") {
            help();
            return std::nullopt;
        }

        if (arg == "--version") {
            version();
            return std::nullopt;
        }

        if (arg == "-O") {
            options.optimize = true;
            continue;
        }

        if (arg == "--run") {
            options.run = true;
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

            return std::nullopt;
        }

        if (options.source_file) {
            cent::log::error("multiple input files provided");
            return std::nullopt;
        }

        options.source_file = arg;
    }

    if (expecting_output) {
        cent::log::error("missing filename");
        return std::nullopt;
    }

    if (expecting_target) {
        cent::log::error("missing target triple");
        return std::nullopt;
    }

    if (expecting_emit_type) {
        cent::log::error("missing emit type");
        return std::nullopt;
    }

    if (!options.source_file) {
        cent::log::error("no input file provided");
        return std::nullopt;
    }

    return options;
}

[[nodiscard]] llvm::TargetMachine* init_llvm(std::string_view triple) {
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    std::string message;

    const auto* target = llvm::TargetRegistry::lookupTarget(triple, message);

    if (!target) {
        cent::log::error(message);
        return nullptr;
    }

    return target->createTargetMachine(
        triple, "generic", "", llvm::TargetOptions{}, llvm::Reloc::PIC_);
}

[[nodiscard]] std::optional<std::filesystem::path>
compile(const Options& options, llvm::TargetMachine* machine) {
    auto code = cent::read_file(*options.source_file);

    if (!code) {
        return std::nullopt;
    }

    std::string filename = options.source_file->string();

    cent::frontend::Parser parser{*code, filename};
    auto program = parser.parse();

    if (!program) {
        return std::nullopt;
    }

    cent::backend::Codegen codegen{
        std::move(program), filename, machine->createDataLayout(),
        options.target_triple};

    auto module = codegen.generate();

    if (!module) {
        return std::nullopt;
    }

    if (parser.had_error() || codegen.had_error()) {
        return std::nullopt;
    }

    if (options.optimize) {
        cent::backend::optimize_module(*module, llvm::OptimizationLevel::O3);
    }

    auto get_output_file = [&](std::string_view extension) {
        if (options.output_file) {
            return *options.output_file;
        }

        auto result = *options.source_file;
        result.replace_extension(extension);

        return result;
    };

    switch (options.emit_type) {
    case EmitType::LlvmIr: {
        auto output = get_output_file(".ll");

        if (!cent::backend::emit_llvm_ir(*module, output)) {
            return std::nullopt;
        }

        return output;
    }
    case EmitType::LlvmBc: {
        auto output = get_output_file(".bc");

        if (!cent::backend::emit_llvm_bc(*module, output)) {
            return std::nullopt;
        }

        return output;
    }
    case EmitType::Obj: {
        auto output = get_output_file(".o");

        if (!cent::backend::emit_obj(

                *module, *machine, output)) {
            return std::nullopt;
        }

        return output;
    }
    case EmitType::Exe:
        std::filesystem::path object_file = std::tmpnam(nullptr);

        if (!cent::backend::emit_obj(*module, *machine, object_file)) {
            return std::nullopt;
        }

        auto output = get_output_file("");

        std::vector<std::string> args = {"-o", output, object_file};

        args.insert(
            args.end(), options.linker_options.begin(),
            options.linker_options.end());

        int exit_code = cent::exec_command("gcc", args);

        std::filesystem::remove(object_file);

        if (exit_code == 0) {
            return output;
        }

        return std::nullopt;
    }

    return std::nullopt;
}

int main(int argc, char** argv) {
    auto options =
        parse_args(std::span<char*>{argv, static_cast<std::size_t>(argc)});

    if (!options) {
        return 1;
    }

    auto* machine = init_llvm(options->target_triple);

    auto output = compile(*options, machine);

    if (!output) {
        return 1;
    }

    if (!options->run) {
        return 0;
    }

    if (options->emit_type != EmitType::Exe) {
        cent::log::error("`--run` can only be used with `--emit exe`");
        return 1;
    }

    return cent::exec_command(std::filesystem::absolute(*output), {});
}
