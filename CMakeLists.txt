cmake_minimum_required(VERSION 3.15)
project(centc LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

include(FetchContent)
set(FETCHCONTENT_QUIET OFF)

option(CENTC_STATIC "Build static executable" OFF)

set(
    CENTC_ARCH
    "${CMAKE_HOST_SYSTEM_PROCESSOR}"
    CACHE STRING "Target architecture"
)

set(CENTC_SYSTEM "${CMAKE_SYSTEM_NAME}" CACHE STRING "Target system")
set(CENTC_LLVM_VERSION "20.1.8" CACHE STRING "LLVM version to download")

if(CENTC_SYSTEM STREQUAL "Linux")
    set(LLVM_URL "https://github.com/centlang/llvm-build/releases/download/20.1.8/llvm-linux-${CENTC_ARCH}.tar.xz")
elseif(CENTC_SYSTEM STREQUAL "Windows")
    set(LLVM_URL "https://github.com/centlang/llvm-build/releases/download/20.1.8/llvm-windows-${CENTC_ARCH}.tar.xz")
else()
    message(FATAL_ERROR "Unsupported system")
endif()

message(STATUS "Downloading LLVM...")

FetchContent_Declare(
    llvm
    URL "${LLVM_URL}"
)

FetchContent_MakeAvailable(llvm)

list(APPEND CMAKE_PREFIX_PATH ${llvm_SOURCE_DIR})
find_package(LLVM REQUIRED CONFIG)

llvm_map_components_to_libnames(
    LLVM_LIBS
    core
    aarch64codegen
    amdgpucodegen
    armcodegen
    avrcodegen
    bpfcodegen
    hexagoncodegen
    lanaicodegen
    loongarchcodegen
    mipscodegen
    msp430codegen
    nvptxcodegen
    powerpccodegen
    riscvcodegen
    sparccodegen
    spirvcodegen
    systemzcodegen
    vecodegen
    webassemblycodegen
    x86codegen
    xcorecodegen
)

string(REPLACE " " ";" LLVM_LIBS "${LLVM_LIBS}")

if(CENTC_STATIC)
    set(BUILD_SHARED_LIBS OFF)
    set(CMAKE_EXE_LINKER_FLAGS "-static")
endif()

message(STATUS "Fetching fmt...")

FetchContent_Declare(
    fmt
    GIT_REPOSITORY https://github.com/fmtlib/fmt.git
    GIT_TAG 11.1.4
    GIT_PROGRESS ON
    EXCLUDE_FROM_ALL
)

FetchContent_MakeAvailable(fmt)

file(GLOB_RECURSE SOURCES CONFIGURE_DEPENDS src/*.cpp)
add_executable(centc ${SOURCES})

target_include_directories(centc PRIVATE src ${LLVM_INCLUDE_DIRS})
target_link_libraries(centc PRIVATE ${LLVM_LIBS} fmt::fmt)

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

    set_property(
        TEST "${BUILD_TEST}" PROPERTY
        ENVIRONMENT "CENTPATH=${CMAKE_SOURCE_DIR}/lib"
    )

    add_test(NAME "${RUN_TEST}" COMMAND "${TEST_OUTPUT}")
    set_tests_properties("${RUN_TEST}" PROPERTIES DEPENDS "${BUILD_TEST}")
endforeach()
