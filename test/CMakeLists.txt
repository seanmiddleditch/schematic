find_package(Catch2 CONFIG REQUIRED)

include(CTest)
include(Catch)
include(test.cmake)

add_executable(schematic_test)
target_link_libraries(schematic_test schematic::schematic Catch2::Catch2WithMain)
target_compile_features(schematic_test PRIVATE cxx_std_23)
target_include_directories(schematic_test PRIVATE "${PROJECT_SOURCE_DIR}/schematic/source" "${CMAKE_CURRENT_SOURCE_DIR}")
target_sources(schematic_test PRIVATE
    "embed_tests.h"
    "evaluator.cpp"
    "evaluator.h"
    "test_context.h"
    "test_lexer.cpp"
    "test_matchers.h"
    $<$<BOOL:${SCHEMATIC_PROTOBUF}>:test_serializer.cpp>
    "test_schemas.cpp"
    "test_strings.h"
)
catch_discover_tests(schematic_test)

if(SCHEMATIC_SCHEMAC)
    schematic_add_tests(IMPORT_DIR input/ TESTS
        "input/test01.sat"
        "input/test02.sat"
    )
endif()

schematic_embed_tests(TARGET schematic_test TESTS
    "schemas/annotations.sat"
    "schemas/complete.sat"
    "schemas/enum.sat"
    "schemas/fail/duplicates.sat"
    "schemas/fail/invalid_base_types.sat"
    "schemas/fail/missing_import.sat"
    "schemas/fail/reserved.sat"
    "schemas/initializers.sat"
    "schemas/message.sat"
    "schemas/modifiers.sat"
    "schemas/struct.sat"
    "schemas/sub/imported.sat"
)
