cmake_minimum_required(VERSION 3.15)
project(centc LANGUAGES C CXX)

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

    add_test(NAME "${RUN_TEST}" COMMAND "${TEST_OUTPUT}")
    set_tests_properties("${RUN_TEST}" PROPERTIES DEPENDS "${BUILD_TEST}")
endforeach()
