find_package(Catch2 CONFIG REQUIRED)

include(CTest)
include(Catch)
include(cmake/test.cmake)

add_executable(schematic_test)
target_link_libraries(schematic_test schematic::schematic Catch2::Catch2WithMain)
target_compile_features(schematic_test PRIVATE cxx_std_23)
target_include_directories(schematic_test PRIVATE "${PROJECT_SOURCE_DIR}/schematic/source")
target_compile_definitions(schematic_test PRIVATE "SCHEMATIC_PROTOBUF=$<BOOL:${SCHEMATIC_PROTOBUF}>")
target_sources(schematic_test PRIVATE FILE_SET HEADERS BASE_DIRS include FILES
    "include/embed_tests.h"
    "include/evaluator.h"
    "include/test_context.h"
    "include/test_logger.h"
    "include/test_matchers.h"
    "include/test_strings.h"
)
target_sources(schematic_test PRIVATE
    "evaluator.cpp"
    "lexer_tests.cpp"
    "schema_tests.cpp"
)
catch_discover_tests(schematic_test)

if(SCHEMATIC_SCHEMAC)
    schematic_add_schemac_tests(IMPORT_DIR schemas/ SCHEMAS
        "schemas/complete.sat"
        "schemas/message.sat"
    )
endif()

schematic_embed_schemas(TARGET schematic_test SCHEMAS
    "schemas/alias.sat"
    "schemas/annotations.sat"
    "schemas/array.sat"
    "schemas/builtin.sat"
    "schemas/complete.sat"
    "schemas/enum.sat"
    "schemas/fail/circular_import.sat"
    "schemas/fail/circular_import_child.sat"
    "schemas/fail/duplicates.sat"
    "schemas/fail/invalid_base_types.sat"
    "schemas/fail/invalid_messages.sat"
    "schemas/fail/missing_import.sat"
    "schemas/fail/version_dupes.sat"
    "schemas/initializers.sat"
    "schemas/message.sat"
    "schemas/modifiers.sat"
    "schemas/order.sat"
    "schemas/struct.sat"
    "schemas/sub/imported.sat"
    "schemas/versions.sat"
)
