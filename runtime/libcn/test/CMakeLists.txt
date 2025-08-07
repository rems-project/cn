cmake_minimum_required(VERSION 3.14)
project(libcn_tests)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Enable testing
enable_testing()

# Fetch Google Test
include(FetchContent)
FetchContent_Declare(
    googletest
    GIT_REPOSITORY https://github.com/google/googletest.git
    GIT_TAG v1.17.0
)
FetchContent_MakeAvailable(googletest)

# Include directories
include_directories($ENV{OPAM_SWITCH_PREFIX}/lib/cn/runtime/include)

# Create test executable
add_executable(libcn_tests
    bennet/harness.cpp
    bennet/ownership_domain.cpp
    bennet/rand.cpp
    bennet/tnum.cpp
    bennet/wint.cpp
)

# Disable narrowing warnings
include(CheckCCompilerFlag)
check_c_compiler_flag(-Wc++11-narrowing HAS_NARROWING)

if (HAS_NARROWING)
    target_compile_options(libcn_tests PRIVATE -Wno-c++11-narrowing)
endif (HAS_NARROWING)

# Enable sanitizers
target_compile_options(libcn_tests PRIVATE -fsanitize=address,undefined)
target_link_options(libcn_tests PRIVATE -fsanitize=address,undefined)

# Link against Google Test and libcn
target_link_libraries(libcn_tests
    # GoogleTest
    gtest_main
    # LibCN
    $ENV{OPAM_SWITCH_PREFIX}/lib/cn/runtime/libcn_test.a
    $ENV{OPAM_SWITCH_PREFIX}/lib/cn/runtime/libbennet.a
    $ENV{OPAM_SWITCH_PREFIX}/lib/cn/runtime/libcn_replica.a
    $ENV{OPAM_SWITCH_PREFIX}/lib/cn/runtime/libcn_exec.a
)

# Add tests
include(GoogleTest)
gtest_discover_tests(libcn_tests)

# Set up CTest
add_test(NAME libcn_tests COMMAND libcn_tests)
