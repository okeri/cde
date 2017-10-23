find_program(LLVM_CONFIG NAMES llvm-config ${LLVM_CONFIG})

if (${LLVM_CONFIG} STREQUAL LLVM_CONFIG-NOTFOUND)
  message(FATAL_ERROR "llvm-config not found. Please specify correct LLVM_CONFIG")
else()
  message("-- Configuring Clang with ${LLVM_CONFIG}")
endif()

execute_process(COMMAND ${LLVM_CONFIG} --prefix OUTPUT_VARIABLE LLVM_PREFIX OUTPUT_STRIP_TRAILING_WHITESPACE)

# -I
execute_process(COMMAND ${LLVM_CONFIG} --includedir OUTPUT_VARIABLE CLANG_INCLUDE_DIR OUTPUT_STRIP_TRAILING_WHITESPACE)

# -L
execute_process(COMMAND ${LLVM_CONFIG} --ldflags OUTPUT_VARIABLE CLANG_LIB_DIR OUTPUT_STRIP_TRAILING_WHITESPACE)
string(REPLACE "-L" "" CLANG_LIB_DIR ${CLANG_LIB_DIR})
link_directories(${CLANG_LIB_DIR})

# -l
execute_process(COMMAND ${LLVM_CONFIG} --libs --system-libs OUTPUT_VARIABLE LLVM_LIBRARIES OUTPUT_STRIP_TRAILING_WHITESPACE)
string(REPLACE "\n" " " LLVM_LIBRARIES ${LLVM_LIBRARIES})
set(CLANG_LIBRARIES "-lclangFrontend -lclangAST -lclangBasic -lclangDriver -lclangEdit -lclangLex -lclangParse -lclangSema -lclangAnalysis -lclangSerialization")
set(CLANG_LIBRARIES "${CLANG_LIBRARIES} ${CLANG_LIBRARIES}")

mark_as_advanced(LLVM_PREFIX CLANG_INCLUDE_DIR CLANG_LIBRARIES LLVM_LIBRARIES)
