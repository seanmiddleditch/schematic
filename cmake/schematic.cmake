function(_schematic_append_property TARGET PROPERTY VALUE)
    get_target_property(PROPERTIES ${TARGET} ${PROPERTY})
    if(NOT PROPERTIES)
        set(PROPERTIES "")
    endif()
    list(APPEND PROPERTIES ${VALUE})
    set_target_properties(${TARGET} PROPERTIES ${PROPERTY} "${PROPERTIES}")
endfunction()

function(target_schema_include_dirs TARGET VISIBILITY)
    foreach(DIR ${ARGN})
        cmake_path(ABSOLUTE_PATH DIR BASE_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" NORMALIZE)
        if(DIR)
            if(VISIBILITY STREQUAL PUBLIC OR VISIBILITY STREQUAL INTERFACE)
                _schematic_append_property(${TARGET} INTERFACE_SCHEMA_INCLUDE_DIRS "${DIR}")
            endif()
            if(VISIBILITY STREQUAL PUBLIC OR VISIBILITY STREQUAL PRIVATE)
                _schematic_append_property(${TARGET} SCHEMA_INCLUDE_DIRS "${DIR}")
            endif()
        endif()
    endforeach()

    set_target_properties(${TARGET} PROPERTIES TRANSITIVE_COMPILE_PROPERTIES "SCHEMA_INCLUDE_DIRS")
endfunction()

function(target_schemas TARGET VISIBILITY)
    find_package(schematic REQUIRED)

    foreach(SCHEMA "${ARGN}")
        cmake_path(ABSOLUTE_PATH SCHEMA BASE_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" NORMALIZE OUTPUT_VARIABLE SCHEMA_ABS)

        cmake_path(REPLACE_EXTENSION SCHEMA LAST_ONLY "json" OUTPUT_VARIABLE SCHEMA_JSON)
        cmake_path(ABSOLUTE_PATH SCHEMA_JSON BASE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}" NORMALIZE)

        cmake_path(REPLACE_EXTENSION SCHEMA LAST_ONLY "d" OUTPUT_VARIABLE SCHEMA_DEPS)
        cmake_path(ABSOLUTE_PATH SCHEMA_DEPS BASE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}" NORMALIZE)
        message(STATUS "${SCHEMA} ${SCHEMA_ABS} ${SCHEMA_JSON} ${SCHEMA_DEPS}")

        add_custom_command(
            OUTPUT "${SCHEMA_JSON}"
            COMMAND schematic::schemac
                -Ojson -o "${SCHEMA_JSON}"
                -MF "${SCHEMA_DEPS}"
                -I "$<TARGET_PROPERTY:${TARGET},SCHEMA_INCLUDE_DIRS>"
                -I "$<TARGET_PROPERTY:potato_core,SCHEMA_INCLUDE_DIRS>" # FIXME: need cmake 3.30 transitive dependencies
                -- "${SCHEMA_ABS}"
            MAIN_DEPENDENCY "${SCHEMA_ABS}"
            DEPFILE "${SCHEMA_DEPS}"
            #CODEGEN
        )
        target_sources(${TARGET} PRIVATE FILE_SET HEADERS BASE_DIRS "${CMAKE_CURRENT_BINARY_DIR}" FILES
            "${SCHEMA_JSON}"
        )
    endforeach()
endfunction()
