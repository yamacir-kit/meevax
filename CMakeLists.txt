cmake_minimum_required(VERSION 3.5.1)

# ==============================================================================
#   Project Version
# ==============================================================================
set(MAJOR 0)
set(MAJOR_OFFSET   0)

set(MINOR 1)
set(MINOR_OFFSET 191)

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/.git)
  execute_process(
    COMMAND git rev-list --all --count
    OUTPUT_VARIABLE REVISION_COUNT
    )
  math(EXPR PATCH "${REVISION_COUNT} - ${MAJOR_OFFSET} - ${MINOR_OFFSET}")
else()
  set(PATCH -1)
endif()

project(meevax VERSION ${MAJOR}.${MINOR}.${PATCH} LANGUAGES CXX)

# ==============================================================================
#   CXX Flags
# ==============================================================================
set(CMAKE_CXX_FLAGS "-std=c++17 -Wall -Wextra -pedantic")
set(CMAKE_CXX_FLAGS_DEBUG "-g -Og")
set(CMAKE_CXX_FLAGS_RELEASE "-g -O3 -mtune=native -march=native -DNDEBUG")
set(CMAKE_CXX_EXTENSIONS OFF)
# set(CMAKE_CXX_STANDARD 17) # CMake <= 3.8.2

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

find_package(Boost REQUIRED)

set(${PROJECT_NAME}_CONFIGURE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/configure)
if(EXISTS ${${PROJECT_NAME}_CONFIGURE_DIR}/README.md.cmake)
  configure_file(
    ${${PROJECT_NAME}_CONFIGURE_DIR}/README.md.cmake
    ${CMAKE_CURRENT_SOURCE_DIR}/README.md
    )
endif()

string(TIMESTAMP ${PROJECT_NAME}_BUILD_DATE "%Y/%m/%d %H:%M:%S")

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/.git)
  execute_process(
    COMMAND git rev-parse HEAD
    COMMAND tr -d "\n"
    OUTPUT_VARIABLE ${PROJECT_NAME}_BUILD_HASH
    )
else()
  set(${PROJECT_NAME}_BUILD_HASH "")
endif()

set(${PROJECT_NAME}_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/include)

file(GLOB
  ${PROJECT_NAME}_STANDARD_LIBRARY_SOURCES
  ${CMAKE_CURRENT_SOURCE_DIR}/library/*.cpp
  )

foreach(EACH_SOURCE IN LISTS ${PROJECT_NAME}_STANDARD_LIBRARY_SOURCES)
  string(REGEX REPLACE "^/(.*/)*(.*).cpp$" "meevax-\\2" TARGET_NAME ${EACH_SOURCE})
  list(APPEND ${PROJECT_NAME}_STANDARD_LIBRARIES ${TARGET_NAME})
endforeach()
list(SORT ${PROJECT_NAME}_STANDARD_LIBRARIES)
string(REPLACE ";" ", "
  ${PROJECT_NAME}_STANDARD_LIBRARIES_IN_COMMA_SEPARATED_STRING
  "${${PROJECT_NAME}_STANDARD_LIBRARIES}")
message("${${PROJECT_NAME}_STANDARD_LIBRARIES_IN_COMMA_SEPARATED_STRING}")

if(EXISTS ${${PROJECT_NAME}_CONFIGURE_DIR}/configurator.hpp)
  configure_file(
    ${${PROJECT_NAME}_CONFIGURE_DIR}/configurator.hpp
    ${${PROJECT_NAME}_INCLUDE_DIR}/meevax/system/configurator.hpp
    )
endif()

if(EXISTS ${${PROJECT_NAME}_CONFIGURE_DIR}/uninstall.sh.cmake)
  configure_file(
    ${${PROJECT_NAME}_CONFIGURE_DIR}/uninstall.sh.cmake
    ${CMAKE_CURRENT_SOURCE_DIR}/uninstall.sh
    )
endif()

include_directories(
  ${${PROJECT_NAME}_INCLUDE_DIR}
  ${Boost_INCLUDE_DIRS}
  )

link_directories(
  ${Boost_LIBRARY_DIRS}
  )

# ==============================================================
#   MEEVAX CORE
# ==============================================================
add_library(${PROJECT_NAME}-core SHARED
  ${CMAKE_CURRENT_SOURCE_DIR}/src/core.cpp
  )

set_target_properties(${PROJECT_NAME}-core PROPERTIES
  VERSION   ${PROJECT_VERSION}
  SOVERSION ${PROJECT_VERSION_MAJOR}
  )

set(${PROJECT_NAME}_DEPENDENCIES
  ${PROJECT_NAME}-core;
  dl;
  gmp;
  m;
  mpfr;
  stdc++fs
  )

# ==============================================================
#   MEEVAX STANDARD LIBRARIES (SHARED OBJECTS)
# ==============================================================
foreach(EACH_SOURCE IN LISTS ${PROJECT_NAME}_STANDARD_LIBRARY_SOURCES)
  string(REGEX
    REPLACE "^/(.*/)*(.*).cpp$" "meevax-\\2"
    TARGET_NAME ${EACH_SOURCE}
    )

  list(APPEND ${PROJECT_NAME}_STANDARD_LIBRARIES ${TARGET_NAME})

  add_library(${TARGET_NAME} SHARED ${EACH_SOURCE})

  set_target_properties(${TARGET_NAME} PROPERTIES
    VERSION   ${PROJECT_VERSION}
    SOVERSION ${PROJECT_VERSION_MAJOR}
    )

  target_link_libraries(${TARGET_NAME}
    ${Boost_LIBRARIES}
    ${${PROJECT_NAME}_DEPENDENCIES}
    # ${PROJECT_NAME}-core
    # dl
    # gmp
    # mpfr
    # stdc++fs
    )
endforeach()

# ==============================================================================
#   MEEVAX EXECUTABLE
# ==============================================================================
add_executable(${PROJECT_NAME}
  ${CMAKE_CURRENT_SOURCE_DIR}/src/main.cpp
  )

target_link_libraries(${PROJECT_NAME}
  ${Boost_LIBRARIES}
  ${${PROJECT_NAME}_DEPENDENCIES}
  # ${PROJECT_NAME}-core
  # dl
  # gmp
  # mpfr
  # stdc++fs
  )

# ==============================================================================
#   INSTALLATION
# ==============================================================================
install(
  TARGETS ${PROJECT_NAME}
  RUNTIME DESTINATION bin
  )

