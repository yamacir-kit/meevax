cmake_minimum_required(VERSION 3.22.1) # Ubuntu 22.04 LTS default

project(example VERSION 0.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)

find_package(Meevax REQUIRED) # NOTE: case-insensitive

add_library(${PROJECT_NAME} SHARED ${CMAKE_CURRENT_SOURCE_DIR}/${PROJECT_NAME}.cpp)

target_compile_options(${PROJECT_NAME} PUBLIC
  "-Wno-pointer-bool-conversion"
  "-Wno-return-type-c-linkage"
  )

target_link_libraries(${PROJECT_NAME} PRIVATE Meevax::kernel Meevax::basis)

enable_testing()

add_test(
  NAME ${PROJECT_NAME}
  COMMAND meevax ${CMAKE_CURRENT_SOURCE_DIR}/${PROJECT_NAME}.ss)

set_property(
  TEST ${PROJECT_NAME}
  PROPERTY ENVIRONMENT "LD_LIBRARY_PATH=$ENV{LD_LIBRARY_PATH}:${CMAKE_CURRENT_BINARY_DIR}")

add_custom_target(continuous-integration
  COMMAND ${CMAKE_MAKE_PROGRAM}
  COMMAND ${CMAKE_CTEST_COMMAND})
