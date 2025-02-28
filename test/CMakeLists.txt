find_package(Catch2 CONFIG REQUIRED)

include(CTest)
include(Catch)
include(cmake/test.cmake)

add_executable(schematic_test)
target_link_libraries(schematic_test schematic::schematic Catch2::Catch2WithMain)
target_compile_features(schematic_test PRIVATE cxx_std_23)
target_include_directories(schematic_test PRIVATE "${PROJECT_SOURCE_DIR}/schematic/source")
target_sources(schematic_test PRIVATE FILE_SET HEADERS BASE_DIRS include FILES
    "include/embed_tests.h"
    "include/evaluator.h"
    "include/test_context.h"
    "include/test_matchers.h"
    "include/test_strings.h"
)
target_sources(schematic_test PRIVATE
    "evaluator.cpp"
    "lexer_tests.cpp"
    "run_test.cpp"
)
catch_discover_tests(schematic_test)

schematic_embed_schemas(TARGET schematic_test SCHEMAS
    "schemas/alias.sat"
    "schemas/annotations.sat"
    "schemas/array.sat"
    "schemas/builtin.sat"
    "schemas/enum.sat"
    "schemas/fields.sat"
    "schemas/initializers.sat"
    "schemas/message.sat"
    "schemas/modifiers.sat"
    "schemas/order.sat"
    "schemas/preamble.sat"
    "schemas/struct.sat"
    "schemas/versions.sat"

    "schemas/include/imported.sat"
    "schemas/include/inner.sat"
    "schemas/include/preamble_impl.sat"

    "schemas/fail/circular_import.sat"
    "schemas/fail/circular_import_child.sat"
    "schemas/fail/duplicates.sat"
    "schemas/fail/invalid_base_types.sat"
    "schemas/fail/invalid_messages.sat"
    "schemas/fail/missing_import.sat"
    "schemas/fail/version_dupes.sat"
)
