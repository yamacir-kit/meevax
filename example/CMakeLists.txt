cmake_minimum_required(VERSION 3.10.2) # Ubuntu 18.04 LTS Default

project(test VERSION 0.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)

find_package(Boost  REQUIRED)
find_package(Meevax REQUIRED) # NOTE: case-insensitive

set(LEAK_CHECK valgrind --error-exitcode=1
                        --leak-check=full
                        --quiet
                        --show-leak-kinds=all)

enable_testing()

macro(define_test TEST_NAME)
  add_executable(${TEST_NAME} ${CMAKE_CURRENT_SOURCE_DIR}/src/${TEST_NAME}.cpp)
  target_link_libraries(${TEST_NAME} Boost::boost Meevax::kernel)
  add_test(NAME ${TEST_NAME}
           WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
           COMMAND ${LEAK_CHECK} ./${TEST_NAME})
endmacro()

define_test(test-collector)
define_test(test-list)
define_test(test-r7rs)
