#ifndef CENT_MODULES_H
#define CENT_MODULES_H

#include <filesystem>
#include <optional>

namespace cent {

[[nodiscard]] std::optional<std::filesystem::path>
find_module(std::string_view name) noexcept;

}

#endif
