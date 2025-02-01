cmake_minimum_required(VERSION 3.29)

if (POLICY CMP0141)
  cmake_policy(SET CMP0141 NEW)
  set(CMAKE_MSVC_DEBUG_INFORMATION_FORMAT "$<IF:$<AND:$<C_COMPILER_ID:MSVC>,$<CXX_COMPILER_ID:MSVC>>,$<$<CONFIG:Debug,RelWithDebInfo>:EditAndContinue>,$<$<CONFIG:Debug,RelWithDebInfo>:ProgramDatabase>>")
endif()

project("schematic")

include(CTest)

if(PROJECT_IS_TOP_LEVEL AND BUILD_TESTING)
    set(TESTS_DEFAULT ON)
else()
    set(TEST_DEFAULT OFF)
endif()

option(SCHEMATIC_VERBOSE "Whether extra schematic CMake messages should be displayed" ${PROJECT_IS_TOP_LEVEL})
option(SCHEMATIC_TESTS "Whether tests are built and enabled" ${TESTS_DEFAULT})
option(SCHEMATIC_INSTALL "Whether installation targets are enabled" ${PROJECT_IS_TOP_LEVEL})

if(SCHEMATIC_VERBOSE)
    message(STATUS "Schematic tests: ${SCHEMATIC_TESTS}")
    message(STATUS "Schematic install target: ${SCHEMATIC_INSTALL}")
endif()

if(CMAKE_RUNTIME_OUTPUT_DIRECTORY)
    string(REPLACE "/" "\\\\" SCHEMATIC_OUT_DIR_WIN32 "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}")
    configure_file(launch.vs.json.in "${CMAKE_CURRENT_SOURCE_DIR}/.vs/launch.vs.json" @ONLY NEWLINE_STYLE WIN32)
endif()

set(CMAKE_VS_DEBUGGER_WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")

add_subdirectory(schematic)

if(SCHEMATIC_TESTS)
    include(CTest)
    if(BUILD_TESTING)
        enable_testing()
        add_subdirectory(test)
    endif()
endif()

if(SCHEMATIC_INSTALL)
    install(EXPORT SchematicTargets
        DESTINATION lib/cmake/schematic
        NAMESPACE schematic::
    )

    export(EXPORT SchematicTargets
        FILE "${CMAKE_CURRENT_BINARY_DIR}/cmake/schematicTargets.cmake"
        NAMESPACE schematic::
    )

    include(CMakePackageConfigHelpers)
    configure_package_config_file(SchematicConfig.cmake.in
        ${CMAKE_CURRENT_BINARY_DIR}/schematicConfig.cmake
        INSTALL_DESTINATION lib/cmake/schematic
    )
    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/schematicConfig.cmake
        DESTINATION lib/cmake/schematic
    )
endif()
