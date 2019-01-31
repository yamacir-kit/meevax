cmake_minimum_required(VERSION 3.5.1)

set(MAJOR 0)
set(MAJOR_OFFSET 0)

set(MINOR 1)
set(MINOR_OFFSET 191)

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/.git)
  execute_process(COMMAND git log --oneline COMMAND wc --lines OUTPUT_VARIABLE TOTAL_COMMITS)
  math(EXPR SUBMINOR "${TOTAL_COMMITS} - ${MAJOR_OFFSET} - ${MINOR_OFFSET}")
else()
  set(SUBMINOR 0)
endif()

project(meevax VERSION ${MAJOR}.${MINOR}.${SUBMINOR} LANGUAGES CXX)

set(CMAKE_BUILD_TYPE minsizerel)
set(CMAKE_CXX_FLAGS "-Wall -Wextra -pedantic -std=c++17 -s")
set(CMAKE_CXX_EXTENSIONS OFF)

find_package(Boost REQUIRED)

set(${PROJECT_NAME}_CONFIGURE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/configure)
if(EXISTS ${${PROJECT_NAME}_CONFIGURE_DIR}/README.md.cmake)
  configure_file(${${PROJECT_NAME}_CONFIGURE_DIR}/README.md.cmake ${CMAKE_CURRENT_SOURCE_DIR}/README.md)
endif()

set(${PROJECT_NAME}_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/include)
include_directories(
  ${${PROJECT_NAME}_INCLUDE_DIR}
  ${Boost_INCLUDE_DIRS}
)

link_directories(
  ${Boost_LIBRARY_DIRS}
)

file(GLOB ${PROJECT_NAME}_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp)
# foreach(EACH_SOURCE IN LISTS ${PROJECT_NAME}_SOURCES)
#   string(REGEX REPLACE "^/(.*/)*(.*).cpp$" "\\2" TARGET_NAME ${EACH_SOURCE})
#   add_executable(${TARGET_NAME} ${EACH_SOURCE})
#   target_link_libraries(${TARGET_NAME} ${Boost_LIBRARIES})
# endforeach()

add_executable(${PROJECT_NAME} ${${PROJECT_NAME}_SOURCES})
target_link_libraries(${PROJECT_NAME} ${Boost_LIBRARIES} gmp)

