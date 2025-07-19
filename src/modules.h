#ifndef CENT_MODULES_H
#define CENT_MODULES_H

#include <filesystem>
#include <span>
#include <string>
#include <vector>

namespace cent {

using SearchPath = std::vector<std::filesystem::path>;

[[nodiscard]] std::vector<std::filesystem::path>
find_module(std::span<std::string> path, const SearchPath& search_path);

} // namespace cent

#endif
