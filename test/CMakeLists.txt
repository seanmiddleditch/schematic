find_package(Catch2 CONFIG REQUIRED)

include(CTest)
include(Catch)
add_executable(schematic_test)
target_link_libraries(schematic_test schematic::libschematic Catch2::Catch2WithMain)
target_compile_features(schematic_test PRIVATE cxx_std_23)
target_include_directories(schematic_test PRIVATE "${PROJECT_SOURCE_DIR}/schematic/source")
target_sources(schematic_test PRIVATE
    "schematic_test.cpp"
    "test_context.h"
    "test_matchers.h"
    "test_strings.h"
)
catch_discover_tests(schematic_test)

find_program(DIFF_PATH NAMES diff fc DOC "diff utility for displaying test output mismatch context")

set(_SCHEMA_DIR "${CMAKE_CURRENT_SOURCE_DIR}/schemas")
function(_schematic_add_test TEST)
    add_test(
        NAME schematic_test01
        WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
        COMMAND ${CMAKE_COMMAND}
            "-DEXE=$<TARGET_FILE:schematic::schemac>"
            "-DACCEPT=${CMAKE_CURRENT_SOURCE_DIR}/accept"
            "-DSEARCH=${_SCHEMA_DIR}"
            "-DDIFF=${DIFF_PATH}"
            "-DTEST=${TEST}"
            "-DOUT=${CMAKE_CURRENT_BINARY_DIR}"
            -P "${CMAKE_CURRENT_SOURCE_DIR}/driver.cmake"
    )
endfunction(_schematic_add_test)

_schematic_add_test(test01)
