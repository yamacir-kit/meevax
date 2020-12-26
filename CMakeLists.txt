cmake_minimum_required(VERSION 3.10.2) # Ubuntu 18.04 LTS Default

# ==============================================================================
#   Project Settings
# ==============================================================================

execute_process(
  COMMAND cat ${CMAKE_CURRENT_SOURCE_DIR}/VERSION
  COMMAND tr -d "\n"
  OUTPUT_VARIABLE CURRENT_VERSION
  )

project(meevax VERSION ${CURRENT_VERSION} LANGUAGES CXX)

set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_VERBOSE_MAKEFILE OFF)

set(CMAKE_CXX_FLAGS "-Wall -Wextra -Wpedantic")
set(CMAKE_CXX_FLAGS_DEBUG          "-Og -g")
set(CMAKE_CXX_FLAGS_MINSIZEREL     "-Os    -DNDEBUG")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O2 -g -DNDEBUG")
set(CMAKE_CXX_FLAGS_RELEASE        "-O3    -DNDEBUG -flto")

include(GNUInstallDirs)

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_BINDIR})

find_package(Boost REQUIRED)
find_package(Threads REQUIRED)


# ==============================================================================
#   Configure README
# ==============================================================================

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/configure/README.md)
  configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/configure/README.md
    ${CMAKE_CURRENT_SOURCE_DIR}/README.md)
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

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/configure/feature.hpp)
  configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/configure/feature.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/include/${PROJECT_NAME}/kernel/feature.hpp)
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/configure/version.hpp)
  configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/configure/version.hpp
    ${CMAKE_CURRENT_SOURCE_DIR}/include/${PROJECT_NAME}/kernel/version.hpp)
endif()


# ==============================================================================
#   Convert Scripts to ELF Binary
# ==============================================================================

file(GLOB ${PROJECT_NAME}_BASIS_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/basis/*.ss)

set(${PROJECT_NAME}_BASIS "")

foreach(EACH IN LISTS ${PROJECT_NAME}_BASIS_SOURCES)
  get_filename_component(FILENAME ${EACH} NAME)
  get_filename_component(FILEPATH ${EACH} PATH)

  add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${FILENAME}.o
    WORKING_DIRECTORY ${FILEPATH}
    COMMAND ${CMAKE_OBJCOPY}
    ARGS -I binary
         -O elf64-x86-64
         -B i386
         ${FILENAME}
         ${CMAKE_CURRENT_BINARY_DIR}/${FILENAME}.o)

  list(APPEND ${PROJECT_NAME}_BASIS ${CMAKE_CURRENT_BINARY_DIR}/${FILENAME}.o)
endforeach()


# ==============================================================================
#   Meevax Kernel
# ==============================================================================

add_library(kernel SHARED "")
add_library(Meevax::kernel ALIAS kernel)

target_sources(kernel PRIVATE
  ${${PROJECT_NAME}_BASIS}
  ${CMAKE_CURRENT_SOURCE_DIR}/src/basis.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/boolean.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/character.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/closure.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/continuation.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/error.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/exact_integer.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/ghost.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/list.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/miscellaneous.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/number.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/object.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/pair.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/path.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/port.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/ratio.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/reader.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/string.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/symbol.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/syntactic_continuation.cpp
  ${CMAKE_CURRENT_SOURCE_DIR}/src/vector.cpp
  )

target_include_directories(kernel PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>)

target_link_libraries(kernel PRIVATE
  Boost::boost
  Threads::Threads)

target_link_libraries(kernel PUBLIC
  ${CMAKE_DL_LIBS}
  gmp
  stdc++fs)

set_target_properties(kernel PROPERTIES
  VERSION ${PROJECT_VERSION}
  SOVERSION ${PROJECT_VERSION_MAJOR}
  OUTPUT_NAME ${PROJECT_NAME})

install(
  TARGETS kernel
  EXPORT ${PROJECT_NAME}-config # Generate Configuration
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})

install( # Install generated config.cmake
  EXPORT ${PROJECT_NAME}-config
  EXPORT_LINK_INTERFACE_LIBRARIES
  NAMESPACE Meevax::
  DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME})

install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${CMAKE_INSTALL_INCLUDEDIR}/
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})


# ==============================================================================
#   Meevax Evaluator
# ==============================================================================

add_executable(evaluator
  ${CMAKE_CURRENT_SOURCE_DIR}/src/main.cpp)

target_link_libraries(evaluator
  PRIVATE Meevax::kernel)

set_target_properties(evaluator PROPERTIES
  OUTPUT_NAME ${PROJECT_NAME})

install(
  TARGETS evaluator
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})


# ==============================================================================
#   Meevax Basis Library
# ==============================================================================

file(GLOB
  ${PROJECT_NAME}_BASIS_LIBRARY_SOURCES
  ${CMAKE_CURRENT_SOURCE_DIR}/basis/*.cpp)

foreach(EACH_SOURCE IN LISTS ${PROJECT_NAME}_BASIS_LIBRARY_SOURCES)
  string(REGEX REPLACE "^/(.*/)*(.*).cpp$" "meevax-\\2" TARGET_NAME ${EACH_SOURCE})

  add_library(${TARGET_NAME} SHARED ${EACH_SOURCE})

  set_target_properties(${TARGET_NAME} PROPERTIES
    VERSION ${PROJECT_VERSION}
    SOVERSION ${PROJECT_VERSION_MAJOR})

  target_link_libraries(${TARGET_NAME} PUBLIC Meevax::kernel)

  install(
    TARGETS ${TARGET_NAME}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})
endforeach()


# ==============================================================================
#   Package Version
# ==============================================================================

include(CMakePackageConfigHelpers)

write_basic_package_version_file(
  ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
  COMPATIBILITY SameMajorVersion)

install( # Install Package Version File
  FILES ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
  DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME})


# ==============================================================================
#   make uninstall
# ==============================================================================

add_custom_target(uninstall
  # COMMAND xargs sudo rm -rfv < ${CMAKE_CURRENT_BINARY_DIR}/install_manifest.txt
  COMMAND sudo rm -rfv ${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_BINDIR}/${PROJECT_NAME}
  COMMAND sudo rm -rfv ${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}
  COMMAND sudo rm -rfv ${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/${CMAKE_SHARED_LIBRARY_PREFIX}${PROJECT_NAME}*
  COMMAND sudo rm -rfv ${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME})


# ==============================================================================
#   make test
# ==============================================================================

enable_testing()

add_test(
  NAME r4rs
  COMMAND ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_BINDIR}/meevax
          ${CMAKE_CURRENT_SOURCE_DIR}/basis/experimental/srfi-78.ss
          ${CMAKE_CURRENT_SOURCE_DIR}/test/r4rs.ss)

add_test(
  NAME r7rs
  COMMAND ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_BINDIR}/meevax
          ${CMAKE_CURRENT_SOURCE_DIR}/basis/experimental/srfi-78.ss
          ${CMAKE_CURRENT_SOURCE_DIR}/test/r7rs.ss)

add_test(
  NAME numerical-operations
  COMMAND ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_BINDIR}/meevax
          ${CMAKE_CURRENT_SOURCE_DIR}/basis/experimental/srfi-78.ss
          ${CMAKE_CURRENT_SOURCE_DIR}/test/numerical-operations.ss)

add_test(
  NAME sicp-1
  COMMAND ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_BINDIR}/meevax
          ${CMAKE_CURRENT_SOURCE_DIR}/basis/experimental/srfi-78.ss
          ${CMAKE_CURRENT_SOURCE_DIR}/test/sicp-1.ss)

add_test(
  NAME chibi
  COMMAND ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_BINDIR}/meevax
          ${CMAKE_CURRENT_SOURCE_DIR}/basis/experimental/srfi-78.ss
          ${CMAKE_CURRENT_SOURCE_DIR}/test/chibi-basic.ss)

add_test(
  NAME example
  COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/example/test.sh
          -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
          -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER})
