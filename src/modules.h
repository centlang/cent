#ifndef CENT_MODULES_H
#define CENT_MODULES_H

#include <filesystem>
#include <span>
#include <string>
#include <vector>

namespace cent {

[[nodiscard]] std::vector<std::filesystem::path> find_module(
    std::span<std::string> path, std::span<std::filesystem::path> search_paths);

} // namespace cent

#endif
