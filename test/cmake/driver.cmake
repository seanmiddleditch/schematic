if(NOT EXISTS ${SCHEMAC_PATH})
    message(STATUS "SCHEMAC_PATH must be path to schemac")
    cmake_language(EXIT 1)
endif()
if(NOT EXISTS ${DIFF_PATH} AND NOT EXISTS ${FC_PATH})
    message(STATUS "Either DIFF_PATH or FC_PATH must reference a diff utility")
    cmake_language(EXIT 1)
endif()
if(NOT IS_DIRECTORY ${SEARCH_DIR})
    message(STATUS "SEARCH_DIR must be a valid directory")
    cmake_language(EXIT 1)
endif()
if(NOT IS_DIRECTORY ${OUT_DIR})
    message(STATUS "OUT must be a valid directory")
    cmake_language(EXIT 1)
endif()
if(NOT EXISTS ${INPUT})
    message(STATUS "INPUT must be a schema file")
    cmake_language(EXIT 1)
endif()

get_filename_component(NAME ${INPUT} NAME_WE)
get_filename_component(DIR ${INPUT} DIRECTORY)

cmake_path(CONVERT "${INPUT}" TO_NATIVE_PATH_LIST SOURCE_FILE NORMALIZE)
cmake_path(CONVERT "${OUT_DIR}/${NAME}.json" TO_NATIVE_PATH_LIST OUTPUT_JSON NORMALIZE)
cmake_path(CONVERT "${OUT_DIR}/${NAME}.d" TO_NATIVE_PATH_LIST OUTPUTS_DEPS NORMALIZE)
cmake_path(CONVERT "${DIR}/${NAME}-accept.json" TO_NATIVE_PATH_LIST ACCEPT_JSON NORMALIZE)
cmake_path(CONVERT "${DIR}/${NAME}-accept.d" TO_NATIVE_PATH_LIST ACCEPT_DEPS NORMALIZE)

message(STATUS "Test:    ${NAME}")
message(STATUS "Search:  ${SEARCH_DIR}")
message(STATUS "Input:   ${INPUT}")
message(STATUS "Accept:  ${ACCEPT_JSON}")
message(STATUS "Deps:    ${ACCEPT_DEPS}")
message(STATUS "schemac: ${SCHEMAC_PATH}")
if(DIFF_PATH)
    message(STATUS "Diff:    ${DIFF_PATH}")
else()
    message(STATUS "Fc:      ${FC_PATH}")
endif()

execute_process(
    COMMAND "${SCHEMAC_PATH}" "-I" "${SEARCH_DIR}" "-MF" "${OUTPUTS_DEPS}" -Ojson -o "${OUTPUT_JSON}" -- "${SOURCE_FILE}"
    RESULT_VARIABLE RESULT
    COMMAND_ECHO STDOUT
    ERROR_VARIABLE ERRORS
    TIMEOUT 10
)
if(NOT RESULT EQUAL 0)
    message(STATUS "Error output\n${ERRORS}")
    message(STATUS "Schema compilation failed")
    cmake_language(EXIT 1)
endif()

message(STATUS "Compilation succeeded")

function(test_output EXPECTED ACTUAL)
    message(STATUS "Comparing ${EXPECTED} vs ${ACTUAL}")
    cmake_path(CONVERT "${EXPECTED}" TO_NATIVE_PATH_LIST EXPECTED_NATIVE NORMALIZE)
    cmake_path(CONVERT "${ACTUAL}" TO_NATIVE_PATH_LIST ACTUAL_NATIVE NORMALIZE)
    if(NOT EXISTS "${ACTUAL}")
        message(STATUS "${INPUT} failed: file ${ACTUAL} missing")
        cmake_language(EXIT 1)
    endif()
    execute_process(
        COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol "${ACTUAL_NATIVE}" "${EXPECTED_NATIVE}"
        RESULT_VARIABLE RESULT
    )
    if(NOT RESULT EQUAL 0)
        message(STATUS "Output does not match acceptance test")
        if(DIFF_PATH)
            execute_process(
                COMMAND "${DIFF_PATH}" -u "${EXPECTED_NATIVE}" "${ACTUAL_NATIVE}"
                COMMAND_ECHO STDOUT
                RESULT_VARIABLE RESULT # Without this, execute_process treats a
                                       # non-zero exit as a fatal error
            )
        elseif(FC_PATH)
            execute_process(
                COMMAND "${FC_PATH}" /n "${EXPECTED_NATIVE}" "${ACTUAL_NATIVE}"
                COMMAND_ECHO STDOUT
                RESULT_VARIABLE RESULT # Without this, execute_process treats a
                                       # non-zero exit as a fatal error
            )
        endif()
        cmake_language(EXIT 1)
    endif()
    message(STATUS "Output matches acceptance test")
endfunction()

test_output("${ACCEPT_JSON}" "${OUTPUT_JSON}")

# absolute paths will be different depending on machine, so a simple
# text comparison is not an appropriate way to test this output
#test_output("${ACCEPT_DEPS}" "${OUTPUTS_DEPS}")

cmake_language(EXIT 0)
