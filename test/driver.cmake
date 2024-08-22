if(NOT EXISTS ${EXE})
    message(FATAL_ERROR "EXE must be path to schemac")
endif()
if(NOT IS_DIRECTORY ${SEARCH})
    message(FATAL_ERROR "SEARCH must be a valid directory")
endif()
if(NOT IS_DIRECTORY ${OUT})
    message(FATAL_ERROR "OUT must be a valid directory")
endif()
if(NOT IS_DIRECTORY ${ACCEPT})
    message(FATAL_ERROR "ACCEPT must be a valid directory")
endif()
if(NOT TEST)
    message(FATAL_ERROR "TEST must be a schema file")
endif()

get_filename_component(NAME ${TEST} NAME_WE)

cmake_path(CONVERT "${TEST}" TO_NATIVE_PATH_LIST SOURCE_FILE NORMALIZE)
cmake_path(CONVERT "${OUT}/${NAME}.json" TO_NATIVE_PATH_LIST OUTPUT_FILE NORMALIZE)
cmake_path(CONVERT "${OUT}/${NAME}.d" TO_NATIVE_PATH_LIST DEPS_FILE NORMALIZE)

message(STATUS "Test:   ${NAME}")
message(STATUS "Search: ${SEARCH}")
message(STATUS "Source: ${TEST}")
message(STATUS "Accept: ${ACCEPT}/${NAME}.json")
message(STATUS "Deps:   ${ACCEPT}/${NAME}.d")
if(DIFF)
    message(STATUS "Diff:   ${DIFF}")
endif()

execute_process(
    COMMAND "${EXE}" "-I${SEARCH}" "-MF${DEPS_FILE}" -Ojson -o "${OUTPUT_FILE}" -- "${SOURCE_FILE}"
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
        message(STATUS "${TEST} failed: file ${ACTUAL} missing")
        cmake_language(EXIT 1)
    endif()
    execute_process(
        COMMAND ${CMAKE_COMMAND} -E compare_files --ignore-eol "${ACTUAL_NATIVE}" "${EXPECTED_NATIVE}"
        RESULT_VARIABLE COMPARE
    )
    if(NOT COMPARE EQUAL 0)
        if(DIFF)
            execute_process(
                COMMAND "${DIFF}" "${EXPECTED_NATIVE}" "${ACTUAL_NATIVE}" || exit 0
                COMMAND_ECHO STDOUT
            )
        endif()
        message(STATUS "Output does not match acceptance test")
        cmake_language(EXIT 1)
    endif()
    message(STATUS "- Actual matches expected")
endfunction()

test_output("${ACCEPT}/${NAME}.d" "${DEPS_FILE}")
test_output("${ACCEPT}/${NAME}.json" "${OUTPUT_FILE}")
cmake_language(EXIT 0)
