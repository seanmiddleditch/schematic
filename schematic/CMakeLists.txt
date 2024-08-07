find_package(fmt CONFIG REQUIRED)
find_package(Protobuf CONFIG REQUIRED)

add_library(schematic_libschematic)
add_library(schematic::libschematic ALIAS schematic_libschematic)
target_compile_features(schematic_libschematic PUBLIC cxx_std_23)
target_link_libraries(schematic_libschematic PRIVATE fmt::fmt protobuf::libprotobuf)
target_include_directories(schematic_libschematic PUBLIC include)
target_sources(schematic_libschematic PRIVATE FILE_SET HEADERS FILES
    "include/schematic/arena.h"
    "include/schematic/compile.h"
    "include/schematic/resolver.h"
    "include/schematic/schema.h"
    "include/schematic/serialize.h"
    "include/schematic/source.h"
    "include/schematic/string.h"
)
target_sources(schematic_libschematic PRIVATE
    "schematic.natvis"
    "source/ast.h"
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

add_custom_command(
    OUTPUT
        "${CMAKE_CURRENT_BINARY_DIR}/schematic.pb.h"
        "${CMAKE_CURRENT_BINARY_DIR}/schematic.pb.cc"
    COMMAND
        protobuf::protoc "--cpp_out=${CMAKE_CURRENT_BINARY_DIR}"  "--proto_path=${CMAKE_CURRENT_SOURCE_DIR}" schematic.proto
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
    MAIN_DEPENDENCY schematic.proto
)
target_sources(schematic_libschematic PRIVATE
    "${CMAKE_CURRENT_BINARY_DIR}/schematic.pb.h"
    "${CMAKE_CURRENT_BINARY_DIR}/schematic.pb.cc"
)
target_include_directories(schematic_libschematic PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
