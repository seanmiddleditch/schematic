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

if(NOT ${RESULT} EQUAL 0)
    message(FATAL_ERROR "${TEST} failed: ${RESULT}")
endif()

if(NOT EXISTS "${DEPS_FILE}")
    message(FATAL_ERROR "${TEST} failed: deps file ${DEPS_FILE} missing")
endif()
execute_process(COMMAND
    ${CMAKE_COMMAND} -E compare_files --ignore-eol "${DEPS_FILE}" "${ACCEPT}/${TEST}.d"
    RESULT_VARIABLE DEPS_COMPARE
)
if(NOT DEPS_COMPARE EQUAL 0)
    file(READ "${ACCEPT}/${TEST}.d" EXPECTED)
    file(READ "${DEPS_FILE}" ACTUAL) 
    message("Expected:\n${EXPECTED}\nGot:\n${ACTUAL}")
    message(FATAL_ERROR "Deps output does not match acceptance test")
endif()

if(NOT EXISTS "${OUTPUT_FILE}")
    message(FATAL_ERROR "${TEST} failed: output file ${OUTPUT_FILE} missing")
endif()
execute_process(COMMAND
    ${CMAKE_COMMAND} -E compare_files --ignore-eol "${OUTPUT_FILE}" "${ACCEPT}/${TEST}.json"
    RESULT_VARIABLE OUTPUT_COMPARE
)
if(NOT OUTPUT_COMPARE EQUAL 0)
    file(READ "${ACCEPT}/${TEST}.json" EXPECTED)
    file(READ "${OUTPUT_FILE}" ACTUAL) 
    message("Expected:\n'''${EXPECTED}'''\nGot:\n'''${ACTUAL}'''")
    message(FATAL_ERROR "Output file does not match acceptance test")
endif()
