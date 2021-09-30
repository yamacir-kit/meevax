cmake_minimum_required(VERSION 3.10.2) # Ubuntu 18.04 LTS Default

project(test VERSION 0.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)

find_package(Boost  REQUIRED)
find_package(Meevax REQUIRED) # NOTE: case-insensitive

set(MEMCHECK valgrind --error-exitcode=1
                      --leak-check=full
                      --quiet
                      --show-leak-kinds=all)

macro(define_library LIBRARY_NAME)
  add_library(${LIBRARY_NAME} SHARED ${CMAKE_CURRENT_SOURCE_DIR}/lib/${LIBRARY_NAME}.cpp)
  target_link_libraries(${LIBRARY_NAME} PRIVATE Meevax::kernel)
endmacro()

define_library(example-1)

macro(define_cxx_example TEST_NAME)
  add_executable(${TEST_NAME} ${CMAKE_CURRENT_SOURCE_DIR}/src/${TEST_NAME}.cpp)
  target_link_libraries(${TEST_NAME} PRIVATE Boost::boost
                                     PRIVATE Meevax::kernel)
  add_test(NAME ${TEST_NAME}
           WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
           COMMAND ${MEMCHECK} ./${TEST_NAME})
endmacro()

enable_testing()

define_cxx_example(test-collector)
define_cxx_example(test-r7rs)

macro(define_meevax_example EXAMPLE_NAME)
  add_test(NAME ${EXAMPLE_NAME}
           WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
           COMMAND ${MEMCHECK} meevax ${EXAMPLE_NAME}.ss)
endmacro()

define_meevax_example(foreign-function)
