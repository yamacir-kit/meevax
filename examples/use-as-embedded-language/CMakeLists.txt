cmake_minimum_required(VERSION 3.10.2) # Ubuntu 18.04 LTS Default

project(example VERSION 0.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)

find_package(Meevax REQUIRED)

add_executable(${PROJECT_NAME} ${CMAKE_CURRENT_SOURCE_DIR}/src/main.cpp)

target_link_libraries(${PROJECT_NAME} Meevax::meevax)
