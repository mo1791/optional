cmake_minimum_required(VERSION 3.5)

# Not ideal to use this global variable, but necessary to make sure
# that tooling and projects use the same version
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED TRUE)

# strongly encouraged to enable this globally to avoid conflicts between
# -Wpedantic being enabled and -std=c++2b and -std=gnu++2b for example
# when compiling with PCH enabled
set(CMAKE_CXX_EXTENSIONS OFF)

# Change this to false if you want to disable warnings_as_errors in developer mode
set(OPT_WARNINGS_AS_ERRORS_DEVELOPER_DEFAULT TRUE)

set(CMAKE_EXPORT_COMPILE_COMMANDS TRUE)

project(
    OPTIONAL_CLS
    VERSION 0.0.1
    DESCRIPTION ""
    LANGUAGES CXX
)

add_executable(OPTIONAL main.cxx)