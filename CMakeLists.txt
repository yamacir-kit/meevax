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

set(CMAKE_CXX_FLAGS "-std=c++17 -Wall -Wextra -pedantic")
set(CMAKE_CXX_FLAGS_DEBUG "-g -Og")
set(CMAKE_CXX_FLAGS_RELEASE "-O3 -mtune=native -march=native -g -DNDEBUG")
set(CMAKE_CXX_EXTENSIONS OFF)

find_package(Boost REQUIRED)

set(${PROJECT_NAME}_CONFIGURE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/configure)
if(EXISTS ${${PROJECT_NAME}_CONFIGURE_DIR}/README.md.cmake)
  configure_file(${${PROJECT_NAME}_CONFIGURE_DIR}/README.md.cmake ${CMAKE_CURRENT_SOURCE_DIR}/README.md)
endif()

string(TIMESTAMP ${PROJECT_NAME}_BUILD_DATE "%Y/%m/%d %H:%M:%S")

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/.git)
  execute_process(
    COMMAND git rev-parse HEAD
    COMMAND tr -d "\n"
    OUTPUT_VARIABLE ${PROJECT_NAME}_BUILD_HASH)
else()
  set(${PROJECT_NAME}_BUILD_HASH "build with no git support")
endif()

set(${PROJECT_NAME}_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/include)

if(EXISTS ${${PROJECT_NAME}_CONFIGURE_DIR}/configure.hpp.cmake)
  configure_file(
    ${${PROJECT_NAME}_CONFIGURE_DIR}/configure.hpp.cmake
    ${${PROJECT_NAME}_INCLUDE_DIR}/meevax/configure.hpp)
endif()

include_directories(
  ${${PROJECT_NAME}_INCLUDE_DIR}
  ${Boost_INCLUDE_DIRS}
)

link_directories(
  ${Boost_LIBRARY_DIRS}
)

# ==============================================================
#   MEEVAX CORE LITERAL
# ==============================================================
add_library(${PROJECT_NAME}-literal SHARED
  ${CMAKE_CURRENT_SOURCE_DIR}/src/literal.cpp
)
set_target_properties(${PROJECT_NAME}-literal PROPERTIES
  VERSION   ${PROJECT_VERSION}
  SOVERSION ${PROJECT_VERSION_MAJOR}
)

# ==============================================================
#   MEEVAX STANDARD LIBRARIES (SHARED OBJECTS)
# ==============================================================
file(GLOB ${PROJECT_NAME}_STANDARD_LIBRARY_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/src/standard/*.cpp)
# message(${${PROJECT_NAME}_STANDARD_LIBRARY_SOURCES})

foreach(EACH_SOURCE IN LISTS ${PROJECT_NAME}_STANDARD_LIBRARY_SOURCES)
  string(REGEX REPLACE "^/(.*/)*(.*).cpp$" "meevax-\\2" TARGET_NAME ${EACH_SOURCE})

  list(APPEND ${PROJECT_NAME}_STANDARD_LIBRARIES ${TARGET_NAME})

  add_library(${TARGET_NAME} SHARED ${EACH_SOURCE})
  set_target_properties(${TARGET_NAME} PROPERTIES
    VERSION   ${PROJECT_VERSION}
    SOVERSION ${PROJECT_VERSION_MAJOR}
  )
  target_link_libraries(${TARGET_NAME}
    ${Boost_LIBRARIES}
    ${PROJECT_NAME}-literal
  )
endforeach()

message("${${PROJECT_NAME}_STANDARD_LIBRARIES}") # DEBUG

# ==============================================================
#   MEEVAX EXECUTABLE
# ==============================================================
add_executable(${PROJECT_NAME}
  ${CMAKE_CURRENT_SOURCE_DIR}/src/main.cpp
)
target_link_libraries(${PROJECT_NAME}
  ${Boost_LIBRARIES}
  ${PROJECT_NAME}-literal
  dl
  gmp
  mpfr
  stdc++fs
)

