# CMakeLists.txt

# Specifies the minimum version of CMake required to process this file.
# If the user's CMake version is older, it will stop with an error.
cmake_minimum_required(VERSION 4.0.1)

# Defines the project name ("jupitercc") and its version ("0.0.1").
# This creates several useful variables, like PROJECT_NAME and PROJECT_VERSION.
project(jupitercc VERSION 0.0.1)

# Set a global property to tell the compiler to use the C++23 standard.
# This enables modern C++ features for all targets in the project.
set(CMAKE_CXX_STANDARD 23)

# Add a directory to the list of paths where the compiler will search for
# header files (#include "header.hpp"). This is a global setting.
include_directories(src/include)

# Defines a new executable target named "jupitercc".
# It lists all the source files (.cpp) that need to be compiled and linked
# together to create the final program.
add_executable(jupitercc
    src/main.cpp
    src/codegen.cpp
    src/lexer.cpp
)