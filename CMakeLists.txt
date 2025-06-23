cmake_minimum_required(VERSION 3.15)
project(centc LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

find_package(LLVM REQUIRED CONFIG)
find_package(fmt REQUIRED CONFIG)

file(GLOB_RECURSE SOURCES CONFIGURE_DEPENDS src/*.cpp)
add_executable(centc ${SOURCES})

target_include_directories(centc PRIVATE src)
target_link_libraries(centc PRIVATE LLVM fmt::fmt)

install(TARGETS centc)
