find_package(Catch2 CONFIG REQUIRED)

include(CTest)
include(Catch)
include(test.cmake)

add_executable(schematic_test)
target_link_libraries(schematic_test schematic::schematic Catch2::Catch2WithMain)
target_compile_features(schematic_test PRIVATE cxx_std_23)
target_include_directories(schematic_test PRIVATE "${PROJECT_SOURCE_DIR}/schematic/source")
target_sources(schematic_test PRIVATE
    "test_compiler.cpp"
    "test_context.h"
    "test_lexer.cpp"
    "test_matchers.h"
    $<$<BOOL:${SCHEMATIC_PROTOBUF}>:test_serializer.cpp>
    "test_strings.h"
)
catch_discover_tests(schematic_test)

if(SCHEMATIC_SCHEMAC)
    schematic_add_tests(IMPORT_DIR schemas/ TESTS
        "schemas/test01.sat"
        "schemas/test02.sat"
    )
endif()
