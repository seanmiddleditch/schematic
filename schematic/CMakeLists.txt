find_package(fmt CONFIG REQUIRED)
find_package(Protobuf CONFIG REQUIRED)

add_library(schematic_libschematic_core)
add_library(schematic::libschematic_core ALIAS schematic_libschematic_core)
set_target_properties(schematic_libschematic_core PROPERTIES OUTPUT_NAME libschematic)
target_compile_features(schematic_libschematic_core PUBLIC cxx_std_23)
target_link_libraries(schematic_libschematic_core PRIVATE fmt::fmt)
target_include_directories(schematic_libschematic_core PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)
target_sources(schematic_libschematic_core PUBLIC FILE_SET HEADERS BASE_DIRS include FILES
    "include/schematic/allocator.h"
    "include/schematic/compiler.h"
    "include/schematic/schema.h"
    "include/schematic/serialize.h"
    "include/schematic/utility.h"
)
target_sources(schematic_libschematic_core PRIVATE
    "schematic.natvis"
    "source/allocator.cpp"
    "source/array.h"
    "source/utility.cpp"
)

add_library(schematic_libschematic_compiler)
add_library(schematic::libschematic_compiler ALIAS schematic_libschematic_compiler)
set_target_properties(schematic_libschematic_compiler PROPERTIES OUTPUT_NAME libschematic_compiler)
target_compile_features(schematic_libschematic_compiler PUBLIC cxx_std_23)
target_link_libraries(schematic_libschematic_compiler PUBLIC schematic_libschematic_core PRIVATE fmt::fmt protobuf::libprotobuf)
target_include_directories(schematic_libschematic_compiler PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)
target_sources(schematic_libschematic_compiler PUBLIC FILE_SET HEADERS BASE_DIRS include FILES
    "include/schematic/compiler.h"
)
target_sources(schematic_libschematic_compiler PRIVATE
    "source/array.h"
    "source/ast.h"
    "source/compiler.cpp"
    "source/lexer.cpp"
    "source/lexer.h"
    "source/location.h"
    "source/parser.cpp"
    "source/parser.h"
    "source/token.cpp"
    "source/token.h"
)

add_library(schematic_libschematic_protobuf)
add_library(schematic::libschematic_protobuf ALIAS schematic_libschematic_protobuf)
set_target_properties(schematic_libschematic_protobuf PROPERTIES OUTPUT_NAME libschematic_protobuf)
target_compile_features(schematic_libschematic_protobuf PUBLIC cxx_std_23)
target_link_libraries(schematic_libschematic_protobuf PUBLIC schematic_libschematic_core PRIVATE protobuf::libprotobuf)
target_include_directories(schematic_libschematic_protobuf PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)
target_sources(schematic_libschematic_protobuf PUBLIC FILE_SET HEADERS BASE_DIRS include FILES
    "include/schematic/serialize.h"
)
target_sources(schematic_libschematic_protobuf PRIVATE
    "source/serialize.cpp"
)

add_library(schematic_libschematic INTERFACE)
add_library(schematic::libschematic ALIAS schematic_libschematic)
target_link_libraries(schematic_libschematic INTERFACE schematic_libschematic_core schematic_libschematic_compiler schematic_libschematic_protobuf)

install(
    TARGETS schematic_libschematic_core schematic_libschematic_compiler schematic_libschematic_protobuf
    EXPORT SchematicTargets
    FILE_SET HEADERS
)

file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/protobuf/schematic")
target_include_directories(schematic_libschematic_protobuf PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/protobuf>)
add_custom_command(
    OUTPUT
        "${CMAKE_CURRENT_BINARY_DIR}/protobuf/schematic/schematic.pb.h"
        "${CMAKE_CURRENT_BINARY_DIR}/protobuf/schematic/schematic.pb.cc"
    COMMAND protobuf::protoc "--cpp_out=${CMAKE_CURRENT_BINARY_DIR}/protobuf/schematic" "--proto_path=${CMAKE_CURRENT_SOURCE_DIR}" schematic.proto
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
    MAIN_DEPENDENCY schematic.proto
)
target_sources(schematic_libschematic_protobuf PUBLIC FILE_SET HEADERS BASE_DIRS "${CMAKE_CURRENT_BINARY_DIR}/protobuf" FILES
    "${CMAKE_CURRENT_BINARY_DIR}/protobuf/schematic/schematic.pb.h"
)
target_sources(schematic_libschematic_protobuf PRIVATE
    "${CMAKE_CURRENT_BINARY_DIR}/protobuf/schematic/schematic.pb.cc"
)
