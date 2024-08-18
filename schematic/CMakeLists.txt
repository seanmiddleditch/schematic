find_package(fmt CONFIG REQUIRED)
find_package(Protobuf CONFIG REQUIRED)

add_library(schematic_libschematic)
add_library(schematic::libschematic ALIAS schematic_libschematic)
set_target_properties(schematic_libschematic PROPERTIES OUTPUT_NAME libschematic)
target_compile_features(schematic_libschematic PUBLIC cxx_std_23)
target_link_libraries(schematic_libschematic PRIVATE fmt::fmt protobuf::libprotobuf)
target_include_directories(schematic_libschematic PUBLIC include)
target_sources(schematic_libschematic PUBLIC FILE_SET HEADERS BASE_DIRS include FILES
    "include/schematic/compiler.h"
    "include/schematic/schema.h"
    "include/schematic/serialize.h"
    "include/schematic/utility.h"
)
target_sources(schematic_libschematic PRIVATE
    "schematic.natvis"
    "source/arena.h"
    "source/ast.h"
    "source/compiler.cpp"
    "source/lexer.cpp"
    "source/lexer.h"
    "source/location.h"
    "source/parser.cpp"
    "source/parser.h"
    "source/serialize.cpp"
    "source/token.cpp"
    "source/token.h"
    "source/utility.cpp"
)

install(TARGETS schematic_libschematic)
install(TARGETS schematic_libschematic FILE_SET HEADERS)

file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/protobuf/schematic")
target_include_directories(schematic_libschematic PUBLIC "${PROTO_INCLUDE_DIR}")
add_custom_command(
    OUTPUT
        "${CMAKE_CURRENT_BINARY_DIR}/protobuf/schematic/schematic.pb.h"
        "${CMAKE_CURRENT_BINARY_DIR}/protobuf/schematic/schematic.pb.cc"
    COMMAND protobuf::protoc "--cpp_out=${CMAKE_CURRENT_BINARY_DIR}/protobuf/schematic" "--proto_path=${CMAKE_CURRENT_SOURCE_DIR}" schematic.proto
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
    MAIN_DEPENDENCY schematic.proto
)
target_sources(schematic_libschematic PUBLIC FILE_SET HEADERS BASE_DIRS "${CMAKE_CURRENT_BINARY_DIR}/protobuf" FILES
    "${CMAKE_CURRENT_BINARY_DIR}/protobuf/schematic/schematic.pb.h"
)
target_sources(schematic_libschematic PRIVATE
    "${CMAKE_CURRENT_BINARY_DIR}/protobuf/schematic/schematic.pb.cc"
)
target_include_directories(schematic_libschematic PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
