find_package(Catch2 CONFIG REQUIRED)

include(CTest)
include(Catch)
add_executable(schematic_test)
target_link_libraries(schematic_test schematic::libschematic Catch2::Catch2WithMain)
target_compile_features(schematic_test PRIVATE cxx_std_23)
target_include_directories(schematic_test PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/../source")
target_sources(schematic_test PRIVATE
    "schematic_test.cpp"
    "schematic_test.h"
)
catch_discover_tests(schematic_test)

set(_SCHEMA_DIR "${CMAKE_CURRENT_SOURCE_DIR}/schemas")
function(_schematic_add_test TEST)
    add_test(
        NAME schematic_test01
        COMMAND ${CMAKE_COMMAND}
            "-DEXE=$<TARGET_FILE:schematic::schemac>"
            "-DSEARCH=${_SCHEMA_DIR}"
            "-DTEST=${TEST}"
            -P "${CMAKE_CURRENT_SOURCE_DIR}/driver.cmake"
    )
endfunction(_schematic_add_test)

_schematic_add_test(test01)
