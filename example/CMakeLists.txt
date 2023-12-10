cmake_minimum_required(VERSION 3.16.3) # Ubuntu 20.04 LTS default

project(example VERSION 0.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)

find_package(Meevax REQUIRED) # NOTE: case-insensitive

add_library(${PROJECT_NAME} SHARED ${CMAKE_CURRENT_SOURCE_DIR}/${PROJECT_NAME}.cpp)

target_compile_options(${PROJECT_NAME} PUBLIC "-Wno-return-type-c-linkage")

target_link_libraries(${PROJECT_NAME} PRIVATE Meevax::kernel)

enable_testing()

add_test(NAME ${PROJECT_NAME}
         COMMAND meevax ${PROJECT_NAME}.ss
         WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

add_custom_target(develop COMMAND ${CMAKE_MAKE_PROGRAM}
                          COMMAND ${CMAKE_CTEST_COMMAND})
