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

cmake_path(CONVERT "${SEARCH}/${TEST}.sat" TO_NATIVE_PATH_LIST SOURCE_FILE NORMALIZE)
cmake_path(CONVERT "${OUT}/${TEST}.json" TO_NATIVE_PATH_LIST OUTPUT_FILE NORMALIZE)
cmake_path(CONVERT "${OUT}/${TEST}.d" TO_NATIVE_PATH_LIST DEPS_FILE NORMALIZE)

execute_process(
    COMMAND "${EXE}" "-I${SEARCH}" "-MF${DEPS_FILE}" -Ojson -o "${OUTPUT_FILE}" -- "${SOURCE_FILE}"
    TIMEOUT 10
    ERROR_VARIABLE ERROR
    ECHO_ERROR_VARIABLE
    RESULT_VARIABLE RESULT
)

function(test_output EXPECTED ACTUAL)
    cmake_path(CONVERT "${EXPECTED}" TO_NATIVE_PATH_LIST EXPECTED_NATIVE NORMALIZE)
    cmake_path(CONVERT "${ACTUAL}" TO_NATIVE_PATH_LIST ACTUAL_NATIVE NORMALIZE)
    if(NOT EXISTS "${ACTUAL}")
        message(FATAL_ERROR "${TEST} failed: file ${ACTUAL} missing")
    endif()
    execute_process(COMMAND
        ${CMAKE_COMMAND} -E compare_files --ignore-eol "${ACTUAL_NATIVE}" "${EXPECTED_NATIVE}"
        RESULT_VARIABLE DEPS_COMPARE
    )
    if(NOT DEPS_COMPARE EQUAL 0)
        if(DIFF)
            execute_process(COMMAND "${DIFF}" "${EXPECTED_NATIVE}" "${ACTUAL_NATIVE}")
        else()
            file(READ "${EXPECTED}" EXPECTED_CONTENTS)
            file(READ "${ACTUAL}" ACTUAL_CONTENTS) 
            message("Expected:\n${EXPECTED_CONTENTS}\nGot:\n${ACTUAL_CONTENTS}")
        endif()
        message(FATAL_ERROR "Output does not match acceptance test")
    endif()
endfunction()

if(NOT ${RESULT} EQUAL 0)
    message(FATAL_ERROR "${TEST} failed: ${RESULT}")
endif()

test_output("${ACCEPT}/${TEST}.d" "${DEPS_FILE}")
test_output("${ACCEPT}/${TEST}.json" "${OUTPUT_FILE}")
