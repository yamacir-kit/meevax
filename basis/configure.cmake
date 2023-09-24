file(GLOB ${PROJECT_NAME}_BASIS_SOURCES ${REPOSITORY_ROOT}/basis/*.ss)

foreach(EACH IN LISTS ${PROJECT_NAME}_BASIS_SOURCES)
  get_filename_component(FILENAME ${EACH} NAME)
  execute_process(
    COMMAND ${REPOSITORY_ROOT}/build/bin/format ${EACH}
    OUTPUT_VARIABLE CONFIGURED_${FILENAME})
endforeach()

configure_file(
  ${REPOSITORY_ROOT}/configure/basis.hpp
  ${REPOSITORY_ROOT}/include/meevax/basis/scheme.hpp)
