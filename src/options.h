#ifndef CENT_OPTIONS_H
#define CENT_OPTIONS_H

#include <llvm/TargetParser/Host.h>

#include <unistd.h>

#include <filesystem>
#include <optional>
#include <string>
#include <vector>

namespace cent {

enum struct EmitType : std::uint8_t { Obj, Exe, LlvmIr, LlvmBc };

struct Options {
    std::string target_triple = llvm::sys::getDefaultTargetTriple();

    std::optional<std::filesystem::path> source_file = std::nullopt;
    std::optional<std::filesystem::path> output_file = std::nullopt;

    std::vector<std::string> linker_options;

    EmitType emit_type = EmitType::Exe;

    bool optimize = false;
    bool verbose = false;
    bool run = false;

    bool colorize = isatty(fileno(stdout));
};

extern Options g_options;

} // namespace cent

#endif
