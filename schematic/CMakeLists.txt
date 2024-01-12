find_package(fmt CONFIG REQUIRED)
find_package(flatbuffers CONFIG REQUIRED)

add_library(schematic_libschematic)
add_library(schematic::libschematic ALIAS schematic_libschematic)
target_compile_features(schematic_libschematic PUBLIC cxx_std_23)
target_link_libraries(schematic_libschematic PRIVATE fmt::fmt)
target_include_directories(schematic_libschematic PUBLIC include)
target_sources(schematic_libschematic PRIVATE FILE_SET HEADERS FILES
    "include/schematic/arena.h"
    "include/schematic/compile.h"
    "include/schematic/resolver.h"
    "include/schematic/schema.h"
    "include/schematic/serialize.h"
    "include/schematic/source.h"
)
target_sources(schematic_libschematic PRIVATE
    "schematic.natvis"
    "source/ast.h"
    "source/builtins.cpp"
    "source/builtins.h"
    "source/compile.cpp"
    "source/lexer.cpp"
    "source/lexer.h"
    "source/parser.cpp"
    "source/parser.h"
    "source/schema.cpp"
    "source/serialize.cpp"
    "source/source.cpp"
    "source/token.cpp"
    "source/token.h"
)

flatbuffers_generate_headers(
    TARGET schematic_flatbuffers
    SCHEMAS schematic.fbs
    FLAGS --reflect-names
)
add_dependencies(schematic_libschematic schematic_flatbuffers)
target_include_directories(schematic_libschematic PRIVATE "${CMAKE_CURRENT_BINARY_DIR}/schematic_flatbuffers")
