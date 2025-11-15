![Cent](assets/logo.svg)

A simple systems programming language.

https://centlang.github.io

## Building from Source

Ensure you have the dependencies installed:

- CMake
- C++ compiler with C++20 support
- Make

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

Create a file named `main.cn`:

```
with std::io;

fn main() {
    io::println("Hello, world!");
}
```

Build an executable and run it:

```sh
centc main.cn
./main
Hello, world!
```

Or use `--run` option:

```sh
centc main.cn --run
Hello, world!
```

## License

[MIT](license)
