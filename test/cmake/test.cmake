include(CMakeParseArguments)

find_program(DIFF_PATH NAMES diff DOC "diff utility for displaying test output mismatch context")
if(NOT DIFF_PATH)
    find_program(FC_PATH NAMES fc DOC "fc utility for displaying test output mismatch context")
endif()

function(schematic_add_schemac_tests)
    cmake_parse_arguments(ARG "" "IMPORT_DIR" "SCHEMAS" ${ARGN})

    get_filename_component(IMPORT_DIR ${ARG_IMPORT_DIR} ABSOLUTE)

    foreach(SCHEMA ${ARG_SCHEMAS})
        get_filename_component(NAME ${SCHEMA} NAME_WE)
        add_test(
            NAME schematic_test_${NAME}
            WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
            COMMAND ${CMAKE_COMMAND}
                "-DSCHEMAC_PATH=$<TARGET_FILE:schematic::schemac>"
                "-DDIFF_PATH=${DIFF_PATH}"
                "-DFC_PATH=${FC_PATH}"
                "-DSEARCH_DIR=${IMPORT_DIR}"
                "-DOUT_DIR=${CMAKE_CURRENT_BINARY_DIR}"
                "-DINPUT=${SCHEMA}"
                -P "${CMAKE_CURRENT_SOURCE_DIR}/cmake/driver.cmake"
        )
    endforeach()
endfunction(schematic_add_schemac_tests)

function(schematic_embed_schemas)
    cmake_parse_arguments(ARG "" "TARGET" "SCHEMAS" ${ARGN})
    if(NOT ARG_TARGET)
        message(FATAL_ERROR "TARGET is required")
    endif()
    set(INCLUDES "")
    set(ENTRIES "")
    foreach(SCHEMA ${ARG_SCHEMAS})
        get_filename_component(NAME ${SCHEMA} NAME_WE)
        set(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/embed_${NAME}.inc")

        add_custom_command(
            OUTPUT ${OUTPUT}
            COMMAND ${CMAKE_COMMAND}
                "-DNAME=${NAME}"
                "-DSOURCE=${CMAKE_CURRENT_SOURCE_DIR}/${SCHEMA}"
                "-DOUTPUT=${OUTPUT}"
                -P "${CMAKE_CURRENT_SOURCE_DIR}/cmake/embed.cmake"
            MAIN_DEPENDENCY "${SCHEMA}"
            DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/cmake/embed.cmake"
        )

        set(INCLUDES "${INCLUDES}#include \"embed_${NAME}.inc\"\n")
        set(ENTRIES "${ENTRIES}    { .name = \"${SCHEMA}\", .source = embed_${NAME}_source },\n")

        target_sources("${ARG_TARGET}" PRIVATE "${OUTPUT}")
    endforeach()

    set(EMBED_TESTS "${CMAKE_CURRENT_BINARY_DIR}/embed_tests.cpp")
    file(WRITE "${EMBED_TESTS}" "// GENERATED -- do not edit!\n#include \"embed_tests.h\"\n${INCLUDES}\nnamespace potato::schematic::test\n{\n  const EmbeddedTest test_embeds[] =\n  {\n${ENTRIES}  };\n  const std::size_t test_embeds_count = sizeof(test_embeds)/sizeof(test_embeds[0]);}\n")
    target_sources("${ARG_TARGET}" PRIVATE "${EMBED_TESTS}")
endfunction(schematic_embed_schemas)
