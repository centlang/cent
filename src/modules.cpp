#include "modules.h"

namespace cent {

std::vector<std::filesystem::path> find_module(
    std::span<std::string> path,
    std::span<std::filesystem::path> search_paths) {
    std::vector<std::filesystem::path> result;

    for (const auto& search_path : search_paths) {
        if (!std::filesystem::is_directory(search_path)) {
            continue;
        }

        std::filesystem::path fs_path = search_path;

        for (auto& name : path) {
            fs_path /= name;
        }

        if (std::filesystem::is_directory(fs_path)) {
            bool has_cn = false;

            for (const auto& entry :
                 std::filesystem::recursive_directory_iterator{fs_path}) {
                if (entry.is_directory()) {
                    continue;
                }

                if (entry.path().extension() == ".cn") {
                    has_cn = true;
                    break;
                }
            }

            if (has_cn) {
                result.push_back(fs_path);
            }
        }

        fs_path.replace_extension(".cn");

        if (std::filesystem::exists(fs_path)) {
            result.push_back(fs_path);
        }
    }

    return result;
}

} // namespace cent
