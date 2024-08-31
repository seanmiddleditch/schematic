set(PREFIX "// GENERATED -- do not edit!\nconstexpr char embed_${NAME}_source[] = R\"---(")
set(SUFFIX ")---\"; // ${NAME}\n")
file(READ "${SOURCE}" SOURCE)
file(WRITE "${OUTPUT}" "${PREFIX}${SOURCE}${SUFFIX}")
