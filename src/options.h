#ifndef CENT_OPTIONS_H
#define CENT_OPTIONS_H

#include <llvm/Support/CodeGen.h>
#include <llvm/TargetParser/Host.h>

#ifdef _WIN32
#include <io.h>
#else
#include <unistd.h>
#endif

#include <filesystem>
#include <optional>
#include <string>
#include <vector>

namespace cent {

enum struct EmitType : std::uint8_t { Obj, Exe, LlvmIr, LlvmBc, Asm };

struct Options {
    std::string target_triple = llvm::sys::getDefaultTargetTriple();

    std::optional<std::filesystem::path> source_file = std::nullopt;
    std::optional<std::filesystem::path> output_file = std::nullopt;

    std::vector<std::string> linker_options;

    EmitType emit_type = EmitType::Exe;
    llvm::Reloc::Model reloc_model = llvm::Reloc::PIC_;

    bool optimize = false;
    bool verbose = false;
    bool run = false;
    bool release = false;

    bool colorize =
#ifdef _WIN32
        _isatty(_fileno(stdout));
#else
        isatty(fileno(stdout));
#endif
};

extern Options g_options;

} // namespace cent

#endif
