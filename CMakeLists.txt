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

set(CMAKE_BUILD_TYPE release)
set(CMAKE_CXX_FLAGS "-Wall -Wextra -pedantic -std=c++17 -s")
# set(CMAKE_BUILD_TYPE debug)
# set(CMAKE_CXX_FLAGS "-Wall -Wextra -pedantic -std=c++17 -pg -fno-omit-frame-pointer")
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

add_library(${PROJECT_NAME}-base SHARED
  ${CMAKE_CURRENT_SOURCE_DIR}/src/meevax.cpp
)
set_target_properties(${PROJECT_NAME}-base PROPERTIES
  VERSION   ${PROJECT_VERSION}
  SOVERSION ${PROJECT_VERSION_MAJOR}
)

add_executable(${PROJECT_NAME}
  ${CMAKE_CURRENT_SOURCE_DIR}/src/main.cpp
)
target_link_libraries(${PROJECT_NAME}
  ${Boost_LIBRARIES}
  ${PROJECT_NAME}-base
  dl
  gmp
)

file(GLOB ${PROJECT_NAME}_SCHEME_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/src/scheme/*.cpp)

foreach(EACH_SOURCE IN LISTS ${PROJECT_NAME}_SCHEME_SOURCES)
  string(REGEX REPLACE "^/(.*/)*(.*).cpp$" "scheme-\\2" TARGET_NAME ${EACH_SOURCE})
  add_library(${TARGET_NAME} SHARED
    ${EACH_SOURCE}
  )
  set_target_properties(${TARGET_NAME} PROPERTIES
    VERSION   ${PROJECT_VERSION}
    SOVERSION ${PROJECT_VERSION_MAJOR}
  )
  target_link_libraries(${TARGET_NAME}
    ${Boost_LIBRARIES}
    ${PROJECT_NAME}-base
    gmp
  )
endforeach()

