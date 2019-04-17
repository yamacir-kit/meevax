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

# set(CMAKE_BUILD_TYPE minsizerel)
# set(CMAKE_CXX_FLAGS "-Wall -Wextra -pedantic -std=c++17 -s")
set(CMAKE_BUILD_TYPE debug)
set(CMAKE_CXX_FLAGS "-Wall -Wextra -pedantic -std=c++17 -g -pg")
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

add_executable(${PROJECT_NAME} ${CMAKE_CURRENT_SOURCE_DIR}/src/main.cpp)

target_link_libraries(${PROJECT_NAME}
  ${Boost_LIBRARIES}
  dl
  gmp
)

file(GLOB ${PROJECT_NAME}_MODULES ${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp)
list(REMOVE_ITEM ${PROJECT_NAME}_MODULES ${CMAKE_CURRENT_SOURCE_DIR}/src/main.cpp)

foreach(EACH_SOURCE IN LISTS ${PROJECT_NAME}_MODULES)
  string(REGEX REPLACE "^/(.*/)*(.*).cpp$" "scheme-\\2" TARGET_NAME ${EACH_SOURCE})

  add_library(${TARGET_NAME} SHARED ${EACH_SOURCE})

  set_target_properties(
    ${TARGET_NAME} PROPERTIES
    VERSION ${PROJECT_VERSION}
    SOVERSION ${PROJECT_VERSION_MAJOR}
  )

  # target_link_libraries(${TARGET_NAME} ${Boost_LIBRARIES} gmp)
endforeach()

