execute_process(
  COMMAND git rev-parse --show-toplevel
  COMMAND tr -d "\n"
  OUTPUT_VARIABLE TOPLEVEL)

execute_process(COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/script/unicode.sh --digit-value OUTPUT_VARIABLE ${PROJECT_NAME}_UNICODE_DIGIT_VALUE)
execute_process(COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/script/unicode.sh --downcase    OUTPUT_VARIABLE ${PROJECT_NAME}_UNICODE_DOWNCASE)
execute_process(COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/script/unicode.sh --property    OUTPUT_VARIABLE ${PROJECT_NAME}_UNICODE_PROPERTY)
execute_process(COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/script/unicode.sh --upcase      OUTPUT_VARIABLE ${PROJECT_NAME}_UNICODE_UPCASE)

file(WRITE ${CMAKE_CURRENT_SOURCE_DIR}/include/${PROJECT_NAME}/unicode/digit_value.hpp "${${PROJECT_NAME}_UNICODE_DIGIT_VALUE}")
file(WRITE ${CMAKE_CURRENT_SOURCE_DIR}/include/${PROJECT_NAME}/unicode/downcase.hpp    "${${PROJECT_NAME}_UNICODE_DOWNCASE}")
file(WRITE ${CMAKE_CURRENT_SOURCE_DIR}/include/${PROJECT_NAME}/unicode/property.hpp    "${${PROJECT_NAME}_UNICODE_PROPERTY}")
file(WRITE ${CMAKE_CURRENT_SOURCE_DIR}/include/${PROJECT_NAME}/unicode/upcase.hpp      "${${PROJECT_NAME}_UNICODE_UPCASE}")
