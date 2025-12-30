#include <chrono>
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
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/TargetParser/Host.h>

#include "frontend/parser.h"

#include "backend/llvm/codegen.h"
#include "backend/llvm/emit.h"

#include "log.h"
#include "options.h"
#include "util.h"
#include "version.h"

void version() {
    fmt::print("Cent v{}.{}\n", CENT_VERSION_MAJOR, CENT_VERSION_MINOR);
}

void help() {
    fmt::print(R"(USAGE: centc [options] file [-- linker options]

OPTIONS:
  -h, --help                Print this help message and exit
  -O                        Enable optimizations
  -o <file>                 Place the output into <file>
  --emit <type>             Specify the compiler output type
  --reloc-model <model>     Specify relocation model
  --color                   Force colored output
  --no-color                Disable colored output
  --run                     Build and run immediately
  -t, --target <triple>     Specify the target triple
  -v, --verbose             Use verbose output
  -V, --version             Print Cent version and exit
)");
}

[[nodiscard]] std::optional<llvm::Reloc::Model>
get_reloc_model(std::string_view model) {
    if (model == "static") {
        return llvm::Reloc::Static;
    }

    if (model == "pic") {
        return llvm::Reloc::PIC_;
    }

    if (model == "dynamic-no-pic") {
        return llvm::Reloc::DynamicNoPIC;
    }

    if (model == "ropi") {
        return llvm::Reloc::ROPI;
    }

    if (model == "rwpi") {
        return llvm::Reloc::RWPI;
    }

    if (model == "ropi-rwpi") {
        return llvm::Reloc::ROPI_RWPI;
    }

    cent::log::error(
        "unrecognized relocation model: {}", cent::log::quoted(model));

    return std::nullopt;
}

[[nodiscard]] std::optional<cent::EmitType>
get_emit_type(std::string_view type) {
    if (type == "obj") {
        return cent::EmitType::Obj;
    }

    if (type == "exe") {
        return cent::EmitType::Exe;
    }

    if (type == "llvm-ir") {
        return cent::EmitType::LlvmIr;
    }

    if (type == "llvm-bc") {
        return cent::EmitType::LlvmBc;
    }

    if (type == "asm") {
        return cent::EmitType::Asm;
    }

    cent::log::error("unrecognized emit type: {}", cent::log::quoted(type));

    return std::nullopt;
}

[[nodiscard]] bool parse_args(std::span<char*> args) {
    if (args.size() < 2) {
        help();
        return false;
    }

    bool expecting_output = false;
    bool expecting_target = false;
    bool expecting_emit_type = false;
    bool expecting_reloc_model = false;

    bool parsing_linker_options = false;

    for (const char* arg_cstr : args.subspan(1)) {
        std::string arg = arg_cstr;

        if (parsing_linker_options) {
            cent::g_options.linker_options.push_back(std::move(arg));
            continue;
        }

        if (expecting_output) {
            cent::g_options.output_file = arg;
            expecting_output = false;
            continue;
        }

        if (expecting_target) {
            cent::g_options.target_triple = arg;
            expecting_target = false;
            continue;
        }

        if (expecting_emit_type) {
            expecting_emit_type = false;

            auto type = get_emit_type(arg);

            if (!type) {
                return false;
            }

            cent::g_options.emit_type = *type;
            continue;
        }

        if (expecting_reloc_model) {
            expecting_reloc_model = false;

            auto model = get_reloc_model(arg);

            if (!model) {
                return false;
            }

            cent::g_options.reloc_model = *model;
            continue;
        }

        if (arg == "-h" || arg == "--help") {
            help();
            return false;
        }

        if (arg == "-V" || arg == "--version") {
            version();
            return false;
        }

        if (arg == "-O") {
            cent::g_options.optimize = true;
            continue;
        }

        if (arg == "-v" || arg == "--verbose") {
            cent::g_options.verbose = true;
            continue;
        }

        if (arg == "--run") {
            cent::g_options.run = true;
            continue;
        }

        if (arg == "--emit-checks") {
            cent::g_options.emit_checks = true;
            continue;
        }

        if (arg == "--color") {
            cent::g_options.colorize = true;
            continue;
        }

        if (arg == "--no-color") {
            cent::g_options.colorize = false;
            continue;
        }

        if (arg == "--emit") {
            expecting_emit_type = true;
            continue;
        }

        if (arg == "--reloc-model") {
            expecting_reloc_model = true;
            continue;
        }

        if (arg == "-o") {
            expecting_output = true;
            continue;
        }

        if (arg == "-t" || arg == "--target") {
            expecting_target = true;
            continue;
        }

        if (arg == "--") {
            parsing_linker_options = true;
            continue;
        }

        if (arg.starts_with('-')) {
            cent::log::error("unrecognized option: {}", cent::log::quoted(arg));
            return false;
        }

        if (cent::g_options.source_file) {
            cent::log::error("multiple input files provided");
            return false;
        }

        cent::g_options.source_file = arg;
    }

    if (expecting_output) {
        cent::log::error("missing filename");
        return false;
    }

    if (expecting_target) {
        cent::log::error("missing target triple");
        return false;
    }

    if (expecting_emit_type) {
        cent::log::error("missing emit type");
        return false;
    }

    if (expecting_reloc_model) {
        cent::log::error("missing relocation model");
        return false;
    }

    if (!cent::g_options.source_file) {
        cent::log::error("no input file provided");
        return false;
    }

    return true;
}

[[nodiscard]] llvm::TargetMachine* init_llvm() {
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmPrinters();

    std::string message;

    const auto* target = llvm::TargetRegistry::lookupTarget(
        cent::g_options.target_triple, message);

    if (!target) {
        cent::log::error("{}", message);
        return nullptr;
    }

    return target->createTargetMachine(
        cent::g_options.target_triple, "generic", "", llvm::TargetOptions{},
        cent::g_options.reloc_model);
}

[[nodiscard]] std::optional<std::filesystem::path>
compile(llvm::TargetMachine* machine) {
    auto code = cent::read_file(*cent::g_options.source_file);

    if (!code) {
        return std::nullopt;
    }

    std::string filename = cent::g_options.source_file->string();

    std::chrono::time_point<std::chrono::steady_clock> step_start_time;

    auto start_step = [&] {
        step_start_time = std::chrono::steady_clock::now();
    };

    auto end_step = [&](std::string_view step) {
        cent::log::verbose(
            "{} finished in {:.3f}s", step,
            std::chrono::duration_cast<std::chrono::duration<double>>(
                std::chrono::steady_clock::now() - step_start_time)
                .count());
    };

    start_step();

    cent::frontend::Parser parser{*code, filename};
    auto program = parser.parse();

    if (!program) {
        return std::nullopt;
    }

    if (!parser.had_error()) {
        end_step("parsing");
    }

    start_step();

    cent::backend::Codegen codegen{
        std::move(program), filename, machine->createDataLayout()};

    auto module = codegen.generate();

    if (!module) {
        return std::nullopt;
    }

    if (parser.had_error() || codegen.had_error()) {
        return std::nullopt;
    }

    end_step("IR codegen");

    start_step();

    cent::backend::optimize_module(
        *module, cent::g_options.optimize ? llvm::OptimizationLevel::O3
                                          : llvm::OptimizationLevel::O0);

    end_step("optimization");

    auto get_output_file = [&](std::string_view extension) {
        if (cent::g_options.output_file) {
            return *cent::g_options.output_file;
        }

        auto result = *cent::g_options.source_file;
        result.replace_extension(extension);

        return result;
    };

    switch (cent::g_options.emit_type) {
    case cent::EmitType::LlvmIr: {
        auto output = get_output_file(".ll");

        if (!cent::backend::emit_llvm_ir(*module, output)) {
            return std::nullopt;
        }

        return output;
    }
    case cent::EmitType::LlvmBc: {
        auto output = get_output_file(".bc");

        if (!cent::backend::emit_llvm_bc(*module, output)) {
            return std::nullopt;
        }

        return output;
    }
    case cent::EmitType::Obj: {
        auto output = get_output_file(".o");

        if (!cent::backend::emit_obj(*module, *machine, output)) {
            return std::nullopt;
        }

        return output;
    }
    case cent::EmitType::Asm: {
        auto output = get_output_file(".s");

        if (!cent::backend::emit_asm(*module, *machine, output)) {
            return std::nullopt;
        }

        return output;
    }
    case cent::EmitType::Exe:
        std::filesystem::path object_file = std::tmpnam(nullptr);

        if (!cent::backend::emit_obj(*module, *machine, object_file)) {
            return std::nullopt;
        }

        auto output = get_output_file("");

        std::vector<std::string> args = {"-o", output, object_file};

        switch (cent::g_options.reloc_model) {
        case llvm::Reloc::Static:
        case llvm::Reloc::ROPI:
        case llvm::Reloc::RWPI:
        case llvm::Reloc::ROPI_RWPI:
            args.emplace_back("-no-pie");
            break;
        default:
            break;
        }

        args.insert(
            args.end(), cent::g_options.linker_options.begin(),
            cent::g_options.linker_options.end());

        start_step();
        int exit_code = cent::exec_command("gcc", args);

        std::filesystem::remove(object_file);

        if (exit_code != 0) {
            return std::nullopt;
        }

        end_step("linking");
        return output;
    }

    return std::nullopt;
}

int main(int argc, char** argv) {
    auto start_time = std::chrono::steady_clock::now();

    if (!parse_args(std::span<char*>{argv, static_cast<std::size_t>(argc)})) {
        return 1;
    }

    auto* machine = init_llvm();
    auto output = compile(machine);

    if (!output) {
        return 1;
    }

    cent::log::verbose(
        "total time: {:.2f}s",
        std::chrono::duration_cast<std::chrono::duration<double>>(
            std::chrono::steady_clock::now() - start_time)
            .count());

    if (!cent::g_options.run) {
        return 0;
    }

    if (cent::g_options.emit_type != cent::EmitType::Exe) {
        cent::log::error("`--run` can only be used with `--emit exe`");
        return 1;
    }

    return cent::exec_command(std::filesystem::absolute(*output), {});
}
