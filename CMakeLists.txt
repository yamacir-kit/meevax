cmake_minimum_required(VERSION 3.5.1) # Ubuntu 16.04 LTS Default

# ==============================================================================
#   Calculate Project Version
# ==============================================================================
set(MAJOR 0)
set(MAJOR_OFFSET   0)

set(MINOR 2)
set(MINOR_OFFSET 1000)

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/.git)
  execute_process(
    COMMAND git rev-list --all --count
    OUTPUT_VARIABLE REVISION
    )
  math(EXPR PATCH "${REVISION} - ${MAJOR_OFFSET} - ${MINOR_OFFSET}")
else()
  set(PATCH 0)
endif()

# ==============================================================================
#   Project Settings
# ==============================================================================
project(meevax VERSION ${MAJOR}.${MINOR}.${PATCH} LANGUAGES CXX)

set(${PROJECT_NAME}_CXX_OPTIMIZATION_FLAGS "                                   \
  -fdevirtualize-at-ltrans                                                     \
  -fdevirtualize-speculatively                                                 \
  -ffloat-store                                                                \
  -flto                                                                        \
  -frename-registers                                                           \
  -march=native                                                                \
  -mfpmath=sse                                                                 \
  -mtune=native                                                                \
  -pipe                                                                        \
  ")

set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)
# set(CMAKE_CXX_STANDARD 17) # CMake <= 3.8.2

set(CMAKE_CXX_FLAGS "-std=c++17 -Wall -Wextra -Wpedantic")
set(CMAKE_CXX_FLAGS_DEBUG          "-O0 -g")
set(CMAKE_CXX_FLAGS_MINSIZEREL     "-Os -s -DNDEBUG")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O2 -g -DNDEBUG")
set(CMAKE_CXX_FLAGS_RELEASE        "-O3 -s -DNDEBUG ${${PROJECT_NAME}_CXX_OPTIMIZATION_FLAGS}")

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/bin)

find_package(Boost REQUIRED)

set(${PROJECT_NAME}_CONFIGURE ${CMAKE_CURRENT_SOURCE_DIR}/configure)
set(${PROJECT_NAME}_INCLUDE   ${CMAKE_CURRENT_SOURCE_DIR}/include)

include_directories(
  ${${PROJECT_NAME}_INCLUDE}
  ${Boost_INCLUDE_DIRS}
  )

link_directories(
  ${Boost_LIBRARY_DIRS}
  )

# ==============================================================================
#   Generate List of Meevax Standard Libraries
# ==============================================================================

# XXX This section is partially duplicated in later builds of the standard library.

file(GLOB
  ${PROJECT_NAME}_STANDARD_LIBRARY_SOURCES
  ${CMAKE_CURRENT_SOURCE_DIR}/library/*.cpp
  )

foreach(EACH_SOURCE IN LISTS ${PROJECT_NAME}_STANDARD_LIBRARY_SOURCES)
  string(REGEX REPLACE "^/(.*/)*(.*).cpp$" "meevax-\\2" TARGET_NAME ${EACH_SOURCE})
  list(APPEND ${PROJECT_NAME}_STANDARD_LIBRARIES ${TARGET_NAME})
endforeach()

list(SORT ${PROJECT_NAME}_STANDARD_LIBRARIES)

# TODO
# Provide an association list of standard library names and shared object names
# to the runtime via the configurator as follows.
#
# (((meevax hoge) . libmeevax-hoge.so)
#  ((meevax fuga) . libmeevax-fuga.so)
#  ((meevax piyo) . libmeevax-piyo.so))
#
string(REPLACE ";" " "
  ${PROJECT_NAME}_STANDARD_LIBRARIES_IN_WHITESPACE_SEPARATED_LIST
  "${${PROJECT_NAME}_STANDARD_LIBRARIES}"
  )

message(STATUS "Meevax standard libraries: ${${PROJECT_NAME}_STANDARD_LIBRARIES_IN_WHITESPACE_SEPARATED_LIST}")

# ==============================================================================
#   Configure README
# ==============================================================================
if(EXISTS ${${PROJECT_NAME}_CONFIGURE}/README.md.cmake)
  configure_file(
    ${${PROJECT_NAME}_CONFIGURE}/README.md.cmake
    ${CMAKE_CURRENT_SOURCE_DIR}/README.md
    )
endif()

# ==============================================================================
#   Configure Headers
# ==============================================================================
string(TIMESTAMP ${PROJECT_NAME}_BUILD_DATE "%Y/%m/%d-%H:%M:%S")

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/.git)
  execute_process(
    COMMAND git rev-parse HEAD
    COMMAND tr -d "\n"
    OUTPUT_VARIABLE ${PROJECT_NAME}_BUILD_HASH)
else()
  set(${PROJECT_NAME}_BUILD_HASH "unspecified")
endif()

if(EXISTS ${${PROJECT_NAME}_CONFIGURE}/feature.hpp.cmake)
  configure_file(
    ${${PROJECT_NAME}_CONFIGURE}/feature.hpp.cmake
    ${${PROJECT_NAME}_INCLUDE}/${PROJECT_NAME}/kernel/feature.hpp)
endif()

if(EXISTS ${${PROJECT_NAME}_CONFIGURE}/version.hpp.cmake)
  configure_file(
    ${${PROJECT_NAME}_CONFIGURE}/version.hpp.cmake
    ${${PROJECT_NAME}_INCLUDE}/${PROJECT_NAME}/kernel/version.hpp)
endif()

# ==============================================================================
#   Build Meevax Kernel (Shared Object)
# ==============================================================================
add_library(${PROJECT_NAME}-kernel SHARED
  ${CMAKE_CURRENT_SOURCE_DIR}/src/character.cpp
  )

set_target_properties(${PROJECT_NAME}-kernel PROPERTIES
  VERSION   ${PROJECT_VERSION}
  SOVERSION ${PROJECT_VERSION_MAJOR}
  )

set(${PROJECT_NAME}_DEPENDENCIES
  ${Boost_LIBRARIES}
  ${PROJECT_NAME}-kernel
  dl
  gmp
  m
  mpfr
  stdc++fs
  )

# ==============================================================================
#   Build Meevax Standard Libraries (Shared Objects)
# ==============================================================================
foreach(EACH_SOURCE IN LISTS ${PROJECT_NAME}_STANDARD_LIBRARY_SOURCES)
  string(REGEX REPLACE "^/(.*/)*(.*).cpp$" "meevax-\\2" TARGET_NAME ${EACH_SOURCE})
  add_library(${TARGET_NAME} SHARED ${EACH_SOURCE})
  set_target_properties(${TARGET_NAME} PROPERTIES
    VERSION   ${PROJECT_VERSION}
    SOVERSION ${PROJECT_VERSION_MAJOR}
    )
  target_link_libraries(${TARGET_NAME}
    ${${PROJECT_NAME}_DEPENDENCIES}
    )
endforeach()

# ==============================================================================
#   Convert Scripts to ELF Binary
# ==============================================================================
file(GLOB
  ${PROJECT_NAME}_LAYER_SOURCES
  ${CMAKE_CURRENT_SOURCE_DIR}/library/*.ss
  )

set(${PROJECT_NAME}_LAYERS "")

foreach(EACH IN LISTS ${PROJECT_NAME}_LAYER_SOURCES)
  message(STATUS "Copying object ${EACH}")

  get_filename_component(FILENAME ${EACH} NAME)
  get_filename_component(FILEPATH ${EACH} PATH)
  # message(STATUS "  NAME := ${FILENAME}")
  # message(STATUS "  PATH := ${FILEPATH}")

  # message(STATUS "  OUTPUT := ${CMAKE_CURRENT_BINARY_DIR}/${FILENAME}.o")
  add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${FILENAME}.o
    WORKING_DIRECTORY ${FILEPATH}
    COMMAND ${CMAKE_OBJCOPY}
    ARGS -I binary -O elf64-x86-64 -B i386 ${FILENAME} ${CMAKE_CURRENT_BINARY_DIR}/${FILENAME}.o
    )

  list(APPEND ${PROJECT_NAME}_LAYERS ${CMAKE_CURRENT_BINARY_DIR}/${FILENAME}.o)
endforeach(EACH)

# ==============================================================================
#   Meevax ICE (Incremental Compiler/Evaluator)
# ==============================================================================
add_executable(ice
  ${CMAKE_CURRENT_SOURCE_DIR}/src/main.cpp
  ${${PROJECT_NAME}_LAYERS}
  )

target_include_directories(ice PRIVATE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:include> # implicitly ${CMAKE_INSTALL_PREFIX}/include (= /usr/local/include)
  )

target_link_libraries(ice
  ${${PROJECT_NAME}_DEPENDENCIES}
  )

# ==============================================================================
#   Installation
# ==============================================================================
# XXX DON'T FORGET TO EXECUTE "sudo ldconfig" after installation
install(
  TARGETS ice
          ${PROJECT_NAME}-kernel
          ${${PROJECT_NAME}_STANDARD_LIBRARIES}
  RUNTIME DESTINATION bin
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
  )

# Install Headers
install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/include/
  DESTINATION include
  )

# ==============================================================================
#   Package Config
# ==============================================================================
# TODO
# install(
#   EXPORT ${PROJECT_NAME}-config
#   NAMESPACE ${PROJECT_NAME}::
#   DESTINATION lib/cmake/${PROJECT_NAME}
#   )

# ==============================================================================
#   Package Version
# ==============================================================================
include(CMakePackageConfigHelpers)

write_basic_package_version_file(
  ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
  COMPATIBILITY SameMajorVersion
  )

install(
  FILES ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
  DESTINATION lib/cmake/${PROJECT_NAME}
  )

# ==============================================================================
#   Uninstallation
# ==============================================================================
add_custom_target(uninstall
  COMMAND xargs rm --verbose < ${CMAKE_CURRENT_BINARY_DIR}/install_manifest.txt
  )

