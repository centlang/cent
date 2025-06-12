#ifndef CENT_MODULES_H
#define CENT_MODULES_H

#include <filesystem>
#include <optional>
#include <span>
#include <string>

namespace cent {

struct ModulePath {
    std::optional<std::filesystem::path> directory;
    std::optional<std::filesystem::path> file;
};

[[nodiscard]] ModulePath find_module(
    std::span<std::string> path, std::span<std::filesystem::path> search_paths);

} // namespace cent

#endif
