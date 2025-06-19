#include "modules.h"

namespace cent {

std::vector<std::filesystem::path> find_module(
    std::span<std::string> path,
    std::span<std::filesystem::path> search_paths) {
    std::vector<std::filesystem::path> result;

    for (const auto& search_path : search_paths) {
        std::filesystem::path fs_path = search_path;

        for (auto& name : path) {
            fs_path /= name;
        }

        if (std::filesystem::is_directory(fs_path)) {
            result.push_back(fs_path);
        }

        fs_path.replace_extension("cn");

        if (std::filesystem::exists(fs_path)) {
            result.push_back(fs_path);
        }
    }

    return result;
}

} // namespace cent
