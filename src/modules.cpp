#include "modules.h"

namespace cent {

std::vector<std::filesystem::path>
find_module(std::span<std::string> path, const SearchPath& search_path) {
    std::vector<std::filesystem::path> result;

    for (const auto& search_path : search_path) {
        if (!std::filesystem::is_directory(search_path)) {
            continue;
        }

        std::filesystem::path fs_path = search_path;

        for (auto& name : path) {
            fs_path /= name;
        }

        fs_path.replace_extension(".cn");

        if (std::filesystem::is_regular_file(fs_path)) {
            result.push_back(fs_path);
        }
    }

    return result;
}

} // namespace cent
