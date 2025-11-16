cmake_minimum_required(VERSION 3.15)
project(centc LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

include(FetchContent)

option(CENTC_ZLIB_SHARED "Link zlib dynamically" OFF)
option(CENTC_ZSTD_SHARED "Link zstd dynamically" OFF)
option(CENTC_NCURSES_SHARED "Link ncurses dynamically" OFF)
option(CENTC_STATIC "Build static executable" OFF)

if(CENTC_STATIC AND (CENTC_ZLIB_SHARED OR CENTC_ZSTD_SHARED OR CENTC_NCURSES_SHARED))
    message(FATAL_ERROR "CENTC_STATIC=ON requires all dependencies to be linked statically")
endif()

message(STATUS "Downloading LLVM...")

FetchContent_Declare(
    llvm
    URL https://github.com/centlang/llvm-build/releases/download/20.1.8/llvm-linux-x86_64.tar.xz
)

FetchContent_MakeAvailable(llvm)

list(APPEND CMAKE_PREFIX_PATH ${llvm_SOURCE_DIR})
find_package(LLVM REQUIRED CONFIG)

find_program(LLVM_CONFIG_EXE llvm-config PATHS "${LLVM_TOOLS_BINARY_DIR}" NO_DEFAULT_PATH)

if(NOT LLVM_CONFIG_EXE)
    message(FATAL_ERROR "llvm-config not found")
endif()

execute_process(
    COMMAND "${LLVM_CONFIG_EXE}" --link-static --libfiles core all-targets
    OUTPUT_VARIABLE LLVM_LIBS
    OUTPUT_STRIP_TRAILING_WHITESPACE
)

string(REPLACE " " ";" LLVM_LIBS "${LLVM_LIBS}")

if(CENTC_ZLIB_SHARED)
    set(ZLIB "z")
else()
    find_library(ZLIB libz.a)

    if(NOT ZLIB)
        message(FATAL_ERROR "Static zlib not found")
    endif()
endif()

if(CENTC_ZSTD_SHARED)
    set(ZSTD "zstd")
else()
    find_library(ZSTD libzstd.a)

    if(NOT ZSTD)
        message(FATAL_ERROR "Static zstd not found")
    endif()
endif()

if(CENTC_NCURSES_SHARED)
    set(NCURSES "ncursesw")
else()
    find_library(NCURSES libncursesw.a)

    if(NOT NCURSES)
        message(FATAL_ERROR "Static ncurses not found")
    endif()
endif()

if(CENTC_STATIC)
    set(BUILD_SHARED_LIBS OFF)
    set(CMAKE_EXE_LINKER_FLAGS "-static")
endif()

message(STATUS "Fetching fmt...")

FetchContent_Declare(
    fmt
    GIT_REPOSITORY https://github.com/fmtlib/fmt.git
    GIT_TAG 11.1.4
)

FetchContent_MakeAvailable(fmt)

file(GLOB_RECURSE SOURCES CONFIGURE_DEPENDS src/*.cpp)
add_executable(centc ${SOURCES})

target_include_directories(centc PRIVATE src ${LLVM_INCLUDE_DIRS})
target_link_libraries(centc PRIVATE ${LLVM_LIBS} ${ZLIB} ${ZSTD} ${NCURSES} fmt::fmt)

install(TARGETS centc)
install(DIRECTORY lib/ DESTINATION lib/cent)

enable_testing()

file(GLOB_RECURSE TEST_SOURCES CONFIGURE_DEPENDS test/*.cn)
set(TEST_OUTPUT_DIR "${CMAKE_CURRENT_BINARY_DIR}/test")

file(MAKE_DIRECTORY ${TEST_OUTPUT_DIR})

foreach(TEST_SOURCE ${TEST_SOURCES})
    get_filename_component(TEST_NAME "${TEST_SOURCE}" NAME_WE)
    get_filename_component(TEST_PATH "${TEST_SOURCE}" ABSOLUTE)

    set(TEST_OUTPUT "${TEST_OUTPUT_DIR}/${TEST_NAME}")

    set(BUILD_TEST "build_${TEST_NAME}")
    set(RUN_TEST "run_${TEST_NAME}")

    add_test(
        NAME "${BUILD_TEST}"
        COMMAND "$<TARGET_FILE:centc>" "${TEST_PATH}" -o "${TEST_OUTPUT}"
    )

    set_property(TEST "${BUILD_TEST}" PROPERTY ENVIRONMENT "CENTPATH=${CMAKE_SOURCE_DIR}/lib")

    add_test(NAME "${RUN_TEST}" COMMAND "${TEST_OUTPUT}")
    set_tests_properties("${RUN_TEST}" PROPERTIES DEPENDS "${BUILD_TEST}")
endforeach()
