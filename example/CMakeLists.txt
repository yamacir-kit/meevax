cmake_minimum_required(VERSION 3.10.2) # Ubuntu 18.04 LTS Default

project(unit-test VERSION 0.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)

find_package(Boost  REQUIRED)
find_package(Meevax REQUIRED)

add_executable(${PROJECT_NAME} ${CMAKE_CURRENT_SOURCE_DIR}/src/main.cpp)

target_link_libraries(${PROJECT_NAME} Boost::boost Meevax::Kernel)

# enable_testing()
#
# set(VALGRIND valgrind --error-exitcode=1
#                       --leak-check=full
#                       --quiet
#                       --show-leak-kinds=all)
#
# add_test(
#   NAME ${PROJECT_NAME}
#   WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
#   COMMAND ${VALGRIND} ./${PROJECT_NAME} --report_level=detailed)
