#include <cstdlib>
#include <cstring>

#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include "log.h"
#include "util.h"

namespace cent {

int exec_command(std::string program, std::vector<std::string> args) {
    std::string command = program;

    for (const auto& arg : args) {
        command += " " + arg;
    }

    log::verbose(fmt::format("running {}", log::quoted(command)));

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
            fmt::format(
                "failed to invoke {}: {}", log::quoted(program),
                std::strerror(errno)));

        std::exit(1);
    }

    int status{};
    waitpid(pid, &status, 0);

    return WEXITSTATUS(status);
}

} // namespace cent
