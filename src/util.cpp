#include <cstdlib>
#include <cstring>

#ifdef _WIN32
#include <array>
#include <windows.h>
#else
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#endif

#include "log.h"
#include "util.h"

namespace cent {

int exec_command(std::string program, std::vector<std::string> args) {
    std::string command = program;

    for (const auto& arg : args) {
        command += " " + arg;
    }

    log::verbose("running {}", log::quoted(command));

#ifdef _WIN32
    STARTUPINFOA si;
    PROCESS_INFORMATION pi;

    ZeroMemory(&si, sizeof(si));
    si.cb = sizeof(si);
    ZeroMemory(&pi, sizeof(pi));

    if (!CreateProcessA(
            NULL, command.data(), NULL, NULL, FALSE, 0, NULL, NULL, &si, &pi)) {

        LPSTR buffer = nullptr;

        size_t size = FormatMessageA(
            FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM |
                FORMAT_MESSAGE_IGNORE_INSERTS,
            NULL, GetLastError(), MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
            (LPSTR)&buffer, 0, NULL);

        std::string message(buffer, size);

        LocalFree(buffer);

        log::error("failed to invoke {}: {}", log::quoted(program), message);

        return 1;
    }

    WaitForSingleObject(pi.hProcess, INFINITE);

    DWORD status;
    GetExitCodeProcess(pi.hProcess, &status);

    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);

    return static_cast<int>(status);
#else
    auto pid = fork();

    if (pid == 0) {
        std::vector<char*> argv;
        argv.reserve(args.size() + 2);

        argv.push_back(program.data());

        for (auto& arg : args) {
            argv.push_back(arg.data());
        }

        argv.push_back(nullptr);

        execvp(program.c_str(), argv.data());

        log::error(
            "failed to invoke {}: {}", log::quoted(program),
            std::strerror(errno));

        std::exit(1);
    }

    int status{};
    waitpid(pid, &status, 0);

    return WEXITSTATUS(status);
#endif
}

std::filesystem::path get_exe_path() {
#ifdef _WIN32
    std::array<char, MAX_PATH> path;
    GetModuleFileNameA(NULL, path.data(), MAX_PATH);
    return path.data();
#else
    return std::filesystem::canonical("/proc/self/exe");
#endif
}

} // namespace cent
