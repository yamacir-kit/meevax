cmake_minimum_required(VERSION 3.16.3) # Ubuntu 20.04 LTS default

execute_process(
  COMMAND cat ${CMAKE_CURRENT_SOURCE_DIR}/VERSION
  COMMAND tr -d "\n"
  OUTPUT_VARIABLE CURRENT_VERSION)

project(meevax DESCRIPTION "A programmable programming language"
               HOMEPAGE_URL "https://github.com/yamacir-kit/meevax"
               LANGUAGES CXX
               VERSION ${CURRENT_VERSION})

set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)
set(CMAKE_VERBOSE_MAKEFILE OFF)

string(JOIN " " AGGRESSIVE_OPTIMIZATION_OPTIONS
  # "-fdata-sections"
  # "-ffunction-sections"
  # "-flto" # This optimization causes a SEGV when compiling with Clang 10.
  # "-fmerge-all-constants" # This optimization is very effective in reducing binary size, but non-standard to the C++ standard.
  # "-march=native" # This optimization causes "Illegal instruction" error (is Valgrind's bug) on CI.
  # "-mtune=native"
  )

# NOTE: The `-gdwarf-4` option is set due to the following issues with Clang 14 and Valgrind versions below 3.20: https://bugzilla.mozilla.org/show_bug.cgi?id=1758782
set(CMAKE_CXX_FLAGS "-Wall -Wextra -Wpedantic -pipe")
set(CMAKE_CXX_FLAGS_DEBUG          "-Og -gdwarf-4")
set(CMAKE_CXX_FLAGS_MINSIZEREL     "-Os           -DNDEBUG")
set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O2 -gdwarf-4 -DNDEBUG")
set(CMAKE_CXX_FLAGS_RELEASE        "-O2           -DNDEBUG ${AGGRESSIVE_OPTIMIZATION_OPTIONS}")

include(GNUInstallDirs)

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_BINDIR})

# ---- Configure ---------------------------------------------------------------

include(TestBigEndian)

TEST_BIG_ENDIAN(IS_BIG_ENDIAN) # Use CMAKE_CXX_BYTE_ORDER if CMake >= 3.20

if(${IS_BIG_ENDIAN})
  set(${PROJECT_NAME}_BYTE_ORDER "big-endian")
else()
  set(${PROJECT_NAME}_BYTE_ORDER "little-endian")
endif()

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/configure/help.txt ${CMAKE_CURRENT_BINARY_DIR}/help.txt)

file(READ ${CMAKE_CURRENT_BINARY_DIR}/help.txt ${PROJECT_NAME}_HELP_TEXT)

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/configure/version.cpp ${CMAKE_CURRENT_SOURCE_DIR}/src/kernel/version.cpp)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/configure/README.md   ${CMAKE_CURRENT_SOURCE_DIR}/README.md)

file(GLOB ${PROJECT_NAME}_BASIS_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/basis/*.ss)

foreach(EACH IN LISTS ${PROJECT_NAME}_BASIS_SOURCES)
  get_filename_component(FILENAME ${EACH} NAME)
  file(READ ${EACH} ${PROJECT_NAME}_BASIS_${FILENAME})
endforeach()

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/configure/basis.cpp ${CMAKE_CURRENT_SOURCE_DIR}/src/kernel/basis.cpp)

# ---- Target kernel -----------------------------------------------------------

add_library(kernel SHARED "")

add_library(${PROJECT_NAME}::kernel ALIAS kernel)

file(GLOB_RECURSE ${PROJECT_NAME}_KERNEL_SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/src/*/*.cpp)

target_sources(kernel PRIVATE ${${PROJECT_NAME}_KERNEL_SOURCES})

target_include_directories(kernel PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>)

target_link_libraries(kernel PRIVATE ${CMAKE_DL_LIBS}
                             PUBLIC gmp)

set_target_properties(kernel PROPERTIES OUTPUT_NAME ${PROJECT_NAME} # Rename libkernel => libmeevax
                                        VERSION     ${PROJECT_VERSION}
                                        SOVERSION   ${PROJECT_VERSION_MAJOR}
                                        LINK_FLAGS_RELEASE -s)

# ---- Target shell ------------------------------------------------------------

add_executable(shell ${CMAKE_CURRENT_SOURCE_DIR}/src/main.cpp)

target_link_libraries(shell PRIVATE kernel)

set_target_properties(shell PROPERTIES OUTPUT_NAME ${PROJECT_NAME} # Rename shell => meevax
                                       LINK_FLAGS_RELEASE -s)

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

find_program(${PROJECT_NAME}_MEMORY_CHECK_COMMAND valgrind)

set(${PROJECT_NAME}_MEMORY_CHECK_COMMAND_OPTIONS --error-exitcode=1 # = EXIT_FAILURE)
                                                 --leak-check=full
                                                 --quiet
                                                 --show-leak-kinds=all)

file(GLOB ${PROJECT_NAME}_TEST_SS ${CMAKE_CURRENT_SOURCE_DIR}/test/*.ss)

foreach(EACH IN LISTS ${PROJECT_NAME}_TEST_SS)
  get_filename_component(FILENAME ${EACH} NAME_WE)
  add_test(NAME ${FILENAME}
           COMMAND ${${PROJECT_NAME}_MEMORY_CHECK_COMMAND}
                   ${${PROJECT_NAME}_MEMORY_CHECK_COMMAND_OPTIONS}
                   ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/meevax
                   ${EACH})
endforeach()

file(GLOB ${PROJECT_NAME}_TEST_CPP ${CMAKE_CURRENT_SOURCE_DIR}/test/*.cpp)

foreach(EACH IN LISTS ${PROJECT_NAME}_TEST_CPP)
  get_filename_component(FILENAME ${EACH} NAME_WE)
  add_executable(assert-${FILENAME} ${EACH})
  target_link_libraries(assert-${FILENAME} PRIVATE kernel)
  add_test(NAME assert-${FILENAME}
           COMMAND ${${PROJECT_NAME}_MEMORY_CHECK_COMMAND}
                   ${${PROJECT_NAME}_MEMORY_CHECK_COMMAND_OPTIONS}
                   ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/assert-${FILENAME})
endforeach()

# ---- Additional Targets ------------------------------------------------------

execute_process(COMMAND nproc OUTPUT_VARIABLE ${PROJECT_NAME}_NPROC)

add_custom_target(install.deb
  COMMAND ${CMAKE_MAKE_PROGRAM} -j${${PROJECT_NAME}_NPROC}
  COMMAND ${CMAKE_MAKE_PROGRAM} package
  COMMAND sudo apt install ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}_${PROJECT_VERSION}_amd64.deb)

add_custom_target(develop
  COMMAND ${CMAKE_MAKE_PROGRAM} -j${${PROJECT_NAME}_NPROC}
  COMMAND ${CMAKE_MAKE_PROGRAM} test ARGS=-j${${PROJECT_NAME}_NPROC}
  COMMAND ${CMAKE_MAKE_PROGRAM} package
  COMMAND sudo apt install ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}_${PROJECT_VERSION}_amd64.deb)
