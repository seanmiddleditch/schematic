find_package(fmt CONFIG REQUIRED)
find_package(Protobuf CONFIG REQUIRED)

add_library(schematic_core)
add_library(schematic::schematic_core ALIAS schematic_core)
set_target_properties(schematic_core PROPERTIES OUTPUT_NAME schematic)
target_compile_features(schematic_core PUBLIC cxx_std_23)
target_link_libraries(schematic_core PRIVATE fmt::fmt)
target_sources(schematic_core PUBLIC FILE_SET HEADERS BASE_DIRS include FILES
    "include/schematic/allocator.h"
    "include/schematic/schema.h"
    "include/schematic/utility.h"
)
target_sources(schematic_core PRIVATE
    "schematic.natvis"
    "source/allocator.cpp"
    "source/array.h"
    "source/utility.cpp"
)

add_library(schematic_compiler)
add_library(schematic::schematic_compiler ALIAS schematic_compiler)
target_compile_features(schematic_compiler PUBLIC cxx_std_23)
target_link_libraries(schematic_compiler PUBLIC schematic_core PRIVATE fmt::fmt protobuf::libprotobuf)
target_sources(schematic_compiler PUBLIC FILE_SET HEADERS BASE_DIRS include FILES
    "include/schematic/compiler.h"
    "include/schematic/logger.h"
)
target_sources(schematic_compiler PRIVATE
    "source/array.h"
    "source/ast.h"
    "source/compiler.cpp"
    "source/generator.cpp"
    "source/generator.h"
    "source/lexer.cpp"
    "source/lexer.h"
    "source/location.h"
    "source/parser.cpp"
    "source/parser.h"
    "source/token.cpp"
    "source/token.h"
)

set(LIBS "schematic_core;schematic_compiler")

if(SCHEMATIC_PROTOBUF)
    add_library(schematic_protobuf)
    add_library(schematic::schematic_protobuf ALIAS schematic_protobuf)
    target_compile_features(schematic_protobuf PUBLIC cxx_std_23)
    target_link_libraries(schematic_protobuf PUBLIC schematic_core PRIVATE protobuf::libprotobuf)
    target_sources(schematic_protobuf PUBLIC FILE_SET HEADERS BASE_DIRS include FILES
        "include/schematic/protobuf.h"
    )
    target_sources(schematic_protobuf PRIVATE
        "source/protobuf.cpp"
    )
    
    file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/protobuf/schematic")
    add_custom_command(
        OUTPUT
            "${CMAKE_CURRENT_BINARY_DIR}/protobuf/schematic/schematic.pb.h"
            "${CMAKE_CURRENT_BINARY_DIR}/protobuf/schematic/schematic.pb.cc"
        COMMAND protobuf::protoc "--error_format=$<IF:$<BOOL:${MSVC}>,msvs,gcc>" "--cpp_out=${CMAKE_CURRENT_BINARY_DIR}/protobuf/schematic" "--proto_path=${CMAKE_CURRENT_SOURCE_DIR}" schematic.proto
        WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
        MAIN_DEPENDENCY schematic.proto
    )
    target_sources(schematic_protobuf PUBLIC FILE_SET HEADERS BASE_DIRS "${CMAKE_CURRENT_BINARY_DIR}/protobuf" FILES
        "${CMAKE_CURRENT_BINARY_DIR}/protobuf/schematic/schematic.pb.h"
    )
    target_sources(schematic_protobuf PRIVATE
        "${CMAKE_CURRENT_BINARY_DIR}/protobuf/schematic/schematic.pb.cc"
    )

    set(LIBS "${LIBS};schematic_protobuf")

    if(SCHEMATIC_INSTALL)
        install(FILES schematic.proto DESTINATION share)
    endif()
endif()

add_library(schematic INTERFACE)
add_library(schematic::schematic ALIAS schematic)
target_link_libraries(schematic INTERFACE ${LIBS})

if(SCHEMATIC_INSTALL)
    install(
        TARGETS ${LIBS}
        EXPORT SchematicTargets
        FILE_SET HEADERS
    )
endif()
