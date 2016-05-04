project("cde")
cmake_minimum_required(VERSION 2.8)

# FIXME
set(LLVM_CONFIG "llvm-config-3.7")

if (NOT DEFINED LIBDB_PATH)
  set(LIBDB_PATH "/usr/include")
endif()

# config
set(CLANG_LIBS "-lclangFrontend -lclangAST -lclangBasic -lclangDriver -lclangEdit -lclangLex -lclangParse -lclangSema -lclangAnalysis -lclangSerialization")

if (DEFINED LIBDB_PATH)
  if (NOT DEFINED LIBDB_INCLUDE_PATH)
    set(LIBDB_INCLUDE_PATH "${LIBDB_PATH}/include")
  endif()
  if (NOT DEFINED LIBDB_LIB_PATH)
    set(LIBDB_LIB_PATH "${LIBDB_PATH}/lib64")
  endif()
endif()

# llvm include dir
execute_process(COMMAND ${LLVM_CONFIG} --includedir OUTPUT_VARIABLE LLVM_INCLUDES OUTPUT_STRIP_TRAILING_WHITESPACE)

# cflags
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -fomit-frame-pointer -std=c++14 -fvisibility-inlines-hidden -fno-exceptions -ffunction-sections -fdata-sections -Wcast-qual -g -fno-rtti")

# llvm libs
execute_process(COMMAND ${LLVM_CONFIG} --libs --system-libs OUTPUT_VARIABLE LLVM_LIBS OUTPUT_STRIP_TRAILING_WHITESPACE)
string(REPLACE "\n" " " LLVM_LIBS ${LLVM_LIBS})
# llvm lib path
execute_process(COMMAND ${LLVM_CONFIG} --ldflags OUTPUT_VARIABLE LLVM_DIRS OUTPUT_STRIP_TRAILING_WHITESPACE)

include_directories(SYSTEM "src" ${LIBDB_INCLUDE_PATH} ${LLVM_INCLUDES})

file(GLOB SRC src/*.cc)
add_executable(cde ${SRC})

target_link_libraries(cde "-L${LIBDB_LIB_PATH} ${LLVM_DIRS} ${CLANG_LIBS} ${LLVM_LIBS} ${CLANG_LIBS} ${LLVM_LIBS} -ldb_cxx")

if (INCLUDE_TESTS)
  enable_testing()
  add_subdirectory(tests)
endif()
