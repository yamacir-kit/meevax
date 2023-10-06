execute_process(
  COMMAND git rev-parse --show-toplevel
  COMMAND tr -d "\n"
  OUTPUT_VARIABLE TOPLEVEL)

file(GLOB ${PROJECT_NAME}_BASIS_SOURCES ${TOPLEVEL}/basis/*.ss)

foreach(EACH IN LISTS ${PROJECT_NAME}_BASIS_SOURCES)
  get_filename_component(FILENAME ${EACH} NAME)
  execute_process(
    COMMAND ${TOPLEVEL}/build/bin/format ${EACH}
    OUTPUT_VARIABLE CONFIGURED_${FILENAME})
endforeach()

configure_file(
  ${TOPLEVEL}/configure/basis.hpp
  ${TOPLEVEL}/include/meevax/basis/scheme.hpp)