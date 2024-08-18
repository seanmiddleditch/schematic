cmake_minimum_required(VERSION 3.26)

if (POLICY CMP0141)
  cmake_policy(SET CMP0141 NEW)
  set(CMAKE_MSVC_DEBUG_INFORMATION_FORMAT "$<IF:$<AND:$<C_COMPILER_ID:MSVC>,$<CXX_COMPILER_ID:MSVC>>,$<$<CONFIG:Debug,RelWithDebInfo>:EditAndContinue>,$<$<CONFIG:Debug,RelWithDebInfo>:ProgramDatabase>>")
endif()

project("schematic")

if(CMAKE_RUNTIME_OUTPUT_DIRECTORY)
    string(REPLACE "/" "\\\\" SCHEMATIC_OUT_DIR_WIN32 "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}")
    configure_file(.vs/launch.vs.json.in "${CMAKE_CURRENT_SOURCE_DIR}/.vs/launch.vs.json" @ONLY NEWLINE_STYLE WIN32)
endif()

set(CMAKE_VS_DEBUGGER_WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")

add_subdirectory(schematic)
add_subdirectory(schemac)

if(PROJECT_IS_TOP_LEVEL)
    include(CTest)
    if(BUILD_TESTING)
        enable_testing()
        add_subdirectory(test)
    endif()
endif()
