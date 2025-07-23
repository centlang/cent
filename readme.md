# Cent

A simple systems programming language.

## Quick Start

### Building from Source

Ensure you have the dependencies installed:

* CMake (at least 3.15)
* LLVM (17.x)
* C++ compiler with C++20 support
* Make
* fmt

Clone the repository:

```sh
git clone https://github.com/centlang/cent && cd cent
```

Build and install:

```sh
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make && sudo make install
```

### "Hello, world!"

Create a file named `hello_world.cn`:

```
with std::io;

fn main() {
    io::println("Hello, world!");
}
```

Build an executable and run it:

```sh
centc hello_world.cn
./hello_world
Hello, world!
```

Or use `--run` option:

```sh
centc hello_world.cn --run
Hello, world!
```
