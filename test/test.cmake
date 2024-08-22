include(CMakeParseArguments)

find_program(DIFF_PATH NAMES diff fc DOC "diff utility for displaying test output mismatch context")

function(schematic_add_tests)
    cmake_parse_arguments(ARG "" "IMPORT_DIR" "TESTS" ${ARGN})

    get_filename_component(IMPORT_DIR ${ARG_IMPORT_DIR} ABSOLUTE)

    foreach(TEST ${ARG_TESTS})
        get_filename_component(NAME ${TEST} NAME_WE)
        get_filename_component(SOURCE ${TEST} ABSOLUTE)
        add_test(
            NAME schematic_test_${NAME}
            WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
            COMMAND ${CMAKE_COMMAND}
                "-DEXE=$<TARGET_FILE:schematic::schemac>"
                "-DACCEPT=${CMAKE_CURRENT_SOURCE_DIR}/accept"
                "-DSEARCH=${IMPORT_DIR}"
                "-DDIFF=${DIFF_PATH}"
                "-DTEST=${TEST}"
                "-DOUT=${CMAKE_CURRENT_BINARY_DIR}"
                -P "${CMAKE_CURRENT_SOURCE_DIR}/driver.cmake"
        )
    endforeach()
endfunction(schematic_add_tests)
