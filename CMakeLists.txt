cmake_minimum_required(VERSION 3.10.2) # Ubuntu 18.04 LTS Default

# ==============================================================================
#   Project Version
# ==============================================================================
set(MAJOR 0)
set(MAJOR_OFFSET   0)

set(MINOR 2)
set(MINOR_OFFSET 1000)

if (EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/.git)
  execute_process(
    COMMAND git rev-list --count --all
    OUTPUT_VARIABLE REVISION)
  math(EXPR PATCH "${REVISION} - ${MAJOR_OFFSET} - ${MINOR_OFFSET}")
else()
  set(PATCH 0)
endif()

# ==============================================================================
#   Project Settings
# ==============================================================================
project(meevax VERSION ${MAJOR}.${MINOR}.${PATCH} LANGUAGES CXX)

set(${PROJECT_NAME}_OPTIMIZATION_OPTIONS_COMMON " \
  -fdata-sections \
  -ffunction-sections \
  -flto \
  -fsized-deallocation \
  -march=native \
  -mtune=native \
  -pipe \
  ")

if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  set(${PROJECT_NAME}_OPTIMIZATION_OPTIONS " \
    -Wno-strict-aliasing \
    -fdevirtualize-at-ltrans \
    -fdevirtualize-speculatively \
    -ffloat-store \
    -frename-registers \
    -mfpmath=sse \
    -s \
    ")
elseif(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  set(${PROJECT_NAME}_OPTIMIZATION_OPTIONS " \
    -Wno-return-type-c-linkage \
    -faligned-allocation \
    -fdouble-square-bracket-attributes \
    -frelaxed-template-template-args \
    ")
else()
  set(${PROJECT_NAME}_OPTIMIZATION_OPTIONS "")
endif()

set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

set(CMAKE_CXX_FLAGS "-Wall -Wextra -Wpedantic")
set(CMAKE_CXX_FLAGS_DEBUG          "-Og -g")
set(CMAKE_CXX_FLAGS_MINSIZEREL     "-Os    -DNDEBUG")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O2 -g -DNDEBUG")
set(CMAKE_CXX_FLAGS_RELEASE        "-O3    -DNDEBUG ${${PROJECT_NAME}_OPTIMIZATION_OPTIONS} \
                                                    ${${PROJECT_NAME}_OPTIMIZATION_OPTIONS_COMMON}")

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/bin)

find_package(Boost REQUIRED)

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
file(GLOB ${PROJECT_NAME}_LAYER_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/standard/*.ss)

set(${PROJECT_NAME}_LAYERS "")

foreach(EACH IN LISTS ${PROJECT_NAME}_LAYER_SOURCES)
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

  list(APPEND ${PROJECT_NAME}_LAYERS ${CMAKE_CURRENT_BINARY_DIR}/${FILENAME}.o)
endforeach(EACH)

# ==============================================================================
#   Build Meevax Kernel (Shared Object)
# ==============================================================================
option(${PROJECT_NAME}_BUILD_AS_STATIC_LIBRARY "Build lib${PROJECT_NAME} as a static library." OFF)

if(${PROJECT_NAME}_BUILD_AS_STATIC_LIBRARY)
  add_library(${PROJECT_NAME} STATIC)
else()
  add_library(${PROJECT_NAME} SHARED)
endif()

add_library(${PROJECT_NAME}::kernel ALIAS ${PROJECT_NAME})

target_sources(${PROJECT_NAME}
  PRIVATE
    ${${PROJECT_NAME}_LAYERS}
    ${CMAKE_CURRENT_SOURCE_DIR}/src/character.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/src/library.cpp
  )

target_include_directories(${PROJECT_NAME}
  PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>)

target_link_libraries(${PROJECT_NAME}
  PUBLIC
    Boost::boost
    dl
    gmp
    m
    mpfr
    pthread
    stdc++fs
  )

set_target_properties(${PROJECT_NAME}
  PROPERTIES
      VERSION ${PROJECT_VERSION}
    SOVERSION ${PROJECT_VERSION_MAJOR}
  )

install( # Install Kernel
  TARGETS ${PROJECT_NAME}
  EXPORT  ${PROJECT_NAME}-config # Generate Configuration
  ARCHIVE DESTINATION lib # Will be installed automatically
  LIBRARY DESTINATION lib # Will be installed automatically
  )

install( # Install config.cmake
  EXPORT ${PROJECT_NAME}-config
  EXPORT_LINK_INTERFACE_LIBRARIES
  NAMESPACE ${PROJECT_NAME}::
  DESTINATION lib/cmake/${PROJECT_NAME})

install( # Install Headers
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/include/
  DESTINATION include)

# ==============================================================================
#   Meevax ICE (Incremental Compiler/Evaluator)
# ==============================================================================
add_executable(ice
  ${CMAKE_CURRENT_SOURCE_DIR}/src/main.cpp)

target_link_libraries(ice
  PRIVATE ${PROJECT_NAME})

install(
  TARGETS ice
  RUNTIME DESTINATION bin)

# ==============================================================================
#   Example
# ==============================================================================
add_executable(example
  ${CMAKE_CURRENT_SOURCE_DIR}/src/example.cpp)

target_link_libraries(example
  PRIVATE
    meevax::kernel)

# ==============================================================================
#   Build Meevax Standard Libraries (Shared Objects)
# ==============================================================================
file(GLOB
  ${PROJECT_NAME}_STANDARD_LIBRARY_SOURCES
  ${CMAKE_CURRENT_SOURCE_DIR}/standard/*.cpp)

foreach(EACH_SOURCE IN LISTS ${PROJECT_NAME}_STANDARD_LIBRARY_SOURCES)
  string(REGEX REPLACE "^/(.*/)*(.*).cpp$" "meevax-\\2" TARGET_NAME ${EACH_SOURCE})

  add_library(${TARGET_NAME} SHARED ${EACH_SOURCE})

  set_target_properties(${TARGET_NAME}
    PROPERTIES
      VERSION ${PROJECT_VERSION}
      SOVERSION ${PROJECT_VERSION_MAJOR})

  target_link_libraries(${TARGET_NAME}
    PUBLIC
      ${PROJECT_NAME})
endforeach()

install( # Will be installed automatically
  TARGETS ${${PROJECT_NAME}_STANDARD_LIBRARIES}
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib)

# ==============================================================================
#   Package Version
# ==============================================================================
include(CMakePackageConfigHelpers)

write_basic_package_version_file(
  ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
  COMPATIBILITY SameMajorVersion)

install( # Install Package Version File
  FILES ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
  DESTINATION lib/cmake/${PROJECT_NAME})

# ==============================================================================
#   Uninstallation
# ==============================================================================
add_custom_target(uninstall
  COMMAND xargs rm --verbose < ${CMAKE_CURRENT_BINARY_DIR}/install_manifest.txt)
