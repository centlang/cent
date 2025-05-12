#include "cent/modules.h"

namespace cent {

ModulePath find_module(
    std::span<std::string> path,
    std::span<std::filesystem::path> search_paths) noexcept {
    for (const auto& search_path : search_paths) {
        std::filesystem::path fs_path = search_path;

        for (auto& name : path) {
            fs_path /= name;
        }

        ModulePath result;

        auto file = fs_path;
        file.replace_extension("cn");

        if (std::filesystem::exists(file)) {
            result.file = std::move(file);
        }

        if (std::filesystem::is_directory(fs_path)) {
            result.directory = fs_path;
        }

        return result;
    }

    return {};
}

} // namespace cent
