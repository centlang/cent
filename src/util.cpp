#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include "util.h"

namespace cent {

int exec_command(std::string program, std::vector<std::string> args) {
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
    }

    int status{};
    waitpid(pid, &status, 0);

    return WEXITSTATUS(status);
}

} // namespace cent
