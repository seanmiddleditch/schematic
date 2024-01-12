find_package(fmt CONFIG REQUIRED)

add_executable(schematic_schemac)
add_executable(schematic::schemac ALIAS schematic_schemac)
target_compile_features(schematic_schemac PUBLIC cxx_std_23)
target_link_libraries(schematic_schemac PRIVATE fmt::fmt schematic::libschematic)
target_sources(schematic_schemac PRIVATE
    "source/main.cpp"
)
