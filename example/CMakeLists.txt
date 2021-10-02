cmake_minimum_required(VERSION 3.10.2) # Ubuntu 18.04 LTS Default

project(example VERSION 0.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)

find_package(Meevax REQUIRED) # NOTE: case-insensitive

add_library(example SHARED ${CMAKE_CURRENT_SOURCE_DIR}/example.cpp)

target_link_libraries(example PRIVATE Meevax::kernel)

enable_testing()

add_test(NAME foreign-function
         COMMAND meevax foreign-function.ss
         WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
