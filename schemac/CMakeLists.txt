find_package(fmt CONFIG REQUIRED)
find_package(Protobuf CONFIG REQUIRED)

add_executable(schematic_schemac)
add_executable(schematic::schemac ALIAS schematic_schemac)
set_target_properties(schematic_schemac PROPERTIES OUTPUT_NAME schemac)
target_compile_features(schematic_schemac PUBLIC cxx_std_23)
target_link_libraries(schematic_schemac PRIVATE fmt::fmt schematic::libschematic protobuf::libprotobuf)
target_compile_definitions(schematic_schemac PRIVATE _SILENCE_STDEXT_ARR_ITERS_DEPRECATION_WARNING=1)
target_sources(schematic_schemac PRIVATE
    "source/main.cpp"
)

install(TARGETS schematic_schemac EXPORT SchematicTargets)
