find_package(fmt CONFIG REQUIRED)

add_library(schematic)
add_library(schematic::schematic ALIAS schematic)
set_target_properties(schematic PROPERTIES OUTPUT_NAME schematic)
target_compile_features(schematic PUBLIC cxx_std_23)
target_link_libraries(schematic PRIVATE fmt::fmt)
target_sources(schematic PUBLIC FILE_SET HEADERS BASE_DIRS include FILES
    "include/schematic/allocator.h"
    "include/schematic/compiler.h"
    "include/schematic/logger.h"
    "include/schematic/schema.h"
    "include/schematic/utility.h"
)
target_sources(schematic PRIVATE
    "schematic.natvis"
    "source/allocator.cpp"
    "source/array.h"
    "source/ast.h"
    "source/compiler.cpp"
    "source/find.h"
    "source/ir.h"
    "source/ir_gen.cpp"
    "source/ir_gen.h"
    "source/lexer.cpp"
    "source/lexer.h"
    "source/location.h"
    "source/parser.cpp"
    "source/parser.h"
    "source/schema_gen.cpp"
    "source/schema_gen.h"
    "source/token.cpp"
    "source/token.h"
    "source/utility.cpp"
)

if(SCHEMATIC_INSTALL)
    install(
        TARGETS schematic
        EXPORT SchematicTargets
        FILE_SET HEADERS
    )
endif()
