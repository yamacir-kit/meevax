cmake_minimum_required(VERSION 3.10.2) # Ubuntu 18.04 LTS Default

execute_process(
  COMMAND cat ${CMAKE_CURRENT_SOURCE_DIR}/VERSION
  COMMAND tr -d "\n"
  OUTPUT_VARIABLE CURRENT_VERSION)

project(meevax DESCRIPTION "A programmable programming language"
               HOMEPAGE_URL "https://github.com/yamacir-kit/meevax"
               LANGUAGES CXX
               VERSION ${CURRENT_VERSION})

include(GNUInstallDirs)
include(TestBigEndian)

set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_VERBOSE_MAKEFILE OFF)

set(CMAKE_CXX_FLAGS "-Wall -Wextra -Wpedantic -Wno-parentheses")
set(CMAKE_CXX_FLAGS_DEBUG          "-Og -g")
set(CMAKE_CXX_FLAGS_MINSIZEREL     "-Os    -DNDEBUG")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O2 -g -DNDEBUG")
set(CMAKE_CXX_FLAGS_RELEASE        "-O3    -DNDEBUG -pipe") # NOTE: -march=native causes "Illegal instruction" error (is Valgrind's bug) on CI.

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_BINDIR})

# ---- Configure ---------------------------------------------------------------

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/configure/README.md)
  configure_file(${CMAKE_CURRENT_SOURCE_DIR}/configure/README.md
                 ${CMAKE_CURRENT_SOURCE_DIR}/README.md)
endif()

string(TIMESTAMP ${PROJECT_NAME}_BUILD_DATE "%Y/%m/%d-%H:%M:%S")

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/.git)
  execute_process(COMMAND git rev-parse HEAD
                  COMMAND tr -d "\n"
                  OUTPUT_VARIABLE ${PROJECT_NAME}_VERSION_EXACT)
else()
  set(${PROJECT_NAME}_VERSION_EXACT "")
endif()

TEST_BIG_ENDIAN(IS_BIG_ENDIAN) # Use CMAKE_CXX_BYTE_ORDER if CMake >= 3.20
if(${IS_BIG_ENDIAN})
  set(${PROJECT_NAME}_BYTE_ORDER "big-endian")
else()
  set(${PROJECT_NAME}_BYTE_ORDER "little-endian")
endif()

if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/configure/version.cpp)
  configure_file(${CMAKE_CURRENT_SOURCE_DIR}/configure/version.cpp
                 ${CMAKE_CURRENT_SOURCE_DIR}/src/kernel/version.cpp)
endif()

# ---- Convert Basis-Library Sources to Binary ---------------------------------

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

# ---- Target kernel -----------------------------------------------------------

add_library(kernel SHARED "")

add_library(${PROJECT_NAME}::kernel ALIAS kernel)

file(GLOB_RECURSE ${PROJECT_NAME}_KERNEL_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/src/*/*.cpp)

target_sources(kernel PRIVATE ${${PROJECT_NAME}_BASIS}
                      PRIVATE ${${PROJECT_NAME}_KERNEL_SOURCES})

target_include_directories(kernel PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>)

target_link_libraries(kernel PRIVATE stdc++fs
                             PRIVATE ${CMAKE_DL_LIBS}
                             PUBLIC gmp)

set_target_properties(
  kernel PROPERTIES OUTPUT_NAME ${PROJECT_NAME} # Rename libkernel => libmeevax
                    VERSION ${PROJECT_VERSION}
                    SOVERSION ${PROJECT_VERSION_MAJOR})

# ---- Target shell ------------------------------------------------------------

add_executable(shell ${CMAKE_CURRENT_SOURCE_DIR}/src/main.cpp)

target_link_libraries(shell PRIVATE kernel)

set_target_properties(shell PROPERTIES OUTPUT_NAME ${PROJECT_NAME}) # Rename shell => meevax

# ---- CMake Package -----------------------------------------------------------

include(CMakePackageConfigHelpers)

write_basic_package_version_file(
  ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
  COMPATIBILITY SameMajorVersion)

# ---- Target install ----------------------------------------------------------

# /usr/lib/libmeevax
install(TARGETS kernel
        EXPORT ${PROJECT_NAME}-config
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR})

# /usr/bin/meevax
install(TARGETS shell
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

# /usr/include/meevax
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/include/
        DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

# /usr/share/meevax/meevax-config.cmake
install(EXPORT ${PROJECT_NAME}-config
        EXPORT_LINK_INTERFACE_LIBRARIES
        DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME}
        NAMESPACE Meevax::)

# /usr/share/meevax/meevax-config-version.cmake
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
        DESTINATION ${CMAKE_INSTALL_DATADIR}/${PROJECT_NAME})

# ---- Target package ----------------------------------------------------------

set(CPACK_GENERATOR DEB)
set(CPACK_RESOURCE_FILE_LICENSE ${CMAKE_CURRENT_SOURCE_DIR}/LICENSE)
set(CPACK_RESOURCE_FILE_README  ${CMAKE_CURRENT_SOURCE_DIR}/README.md)
set(CPACK_THREADS 0)

set(CPACK_DEBIAN_FILE_NAME DEB-DEFAULT)
set(CPACK_DEBIAN_PACKAGE_MAINTAINER "Tatsuya Yamasaki")
set(CPACK_DEBIAN_PACKAGE_SHLIBDEPS ON)

include(CPack)

# ---- Target test -------------------------------------------------------------

enable_testing()

macro(check TEST_NAME)
  add_test(NAME ${TEST_NAME}
           COMMAND valgrind --error-exitcode=1 # = EXIT_FAILURE
                            --leak-check=full
                            --quiet
                            --show-leak-kinds=all
                   ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/meevax
                   ${CMAKE_CURRENT_SOURCE_DIR}/test/${TEST_NAME}.ss)
endmacro()

check(abandoned)
check(chibi-basic)
check(low-level-macro-facility)
check(numerical-operations)
check(r4rs)
check(r4rs-appendix)
check(r7rs)
check(sicp-1)

file(GLOB ${PROJECT_NAME}_TEST_CXX ${CMAKE_CURRENT_SOURCE_DIR}/test/*.cpp)
foreach(FILEPATH IN LISTS ${PROJECT_NAME}_TEST_CXX)
  get_filename_component(FILENAME ${FILEPATH} NAME_WE)
  add_executable(assert-${FILENAME} ${FILEPATH})
  target_link_libraries(assert-${FILENAME} PRIVATE kernel)
  add_test(NAME assert-${FILENAME}
           COMMAND valgrind --error-exitcode=1 # = EXIT_FAILURE
                            --leak-check=full
                            --quiet
                            --show-leak-kinds=all
                   ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/assert-${FILENAME})
endforeach()

# ---- Additional Targets ------------------------------------------------------

execute_process(COMMAND nproc OUTPUT_VARIABLE NPROC)

add_custom_target(install.deb
  COMMAND make -j ${NPROC}
  COMMAND ${CMAKE_CPACK_COMMAND}
  COMMAND sudo apt install ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}_${PROJECT_VERSION}_amd64.deb)

add_custom_target(safe-install.deb
  COMMAND make -j ${NPROC}
  COMMAND ${CMAKE_CTEST_COMMAND} -j ${NPROC}
  COMMAND ${CMAKE_CPACK_COMMAND}
  COMMAND sudo apt install ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}_${PROJECT_VERSION}_amd64.deb)
