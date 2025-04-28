#include "cent/modules.h"

namespace cent {

std::optional<std::filesystem::path>
find_module(std::string_view name) noexcept {
    auto filename = std::string{name} + ".cn";

    for (const auto& entry : std::filesystem::directory_iterator{"."}) {
        if (entry.path().filename() == filename) {
            return entry;
        }
    }

    return std::nullopt;
}

} // namespace cent
