# Accepted config defines:
# BDB: BDB_ROOT | BDB_INCLUDE_DIR
# CLANG: LLVM_CONFIG
# OPTIONAL: INCLUDE_TESTS
# any other cmake standard defines like CMAKE_CXX_COMPILER BOOST_ROOT

project("cde")
cmake_minimum_required(VERSION 3.1.0)
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_STANDARD 17)

find_package(BDB REQUIRED)
find_package(Clang REQUIRED)

add_definitions(-D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -DCONFIGURED_LLVM_PREFIX="${LLVM_PREFIX}")

include_directories(SYSTEM "src" "src/rapidjson/include" ${BDB_INCLUDE_DIR} ${CLANG_INCLUDE_DIR})

add_library(fileutil src/fileutil.cc)
add_executable(cde
  src/main.cc
  src/cdeproject.cc
  src/cdeindex.cc
  src/emacsmapper.cc
  src/gccsupport.cc)

set_property(TARGET cde PROPERTY COMPILE_FLAGS "-fomit-frame-pointer -fvisibility-inlines-hidden -ffunction-sections -fdata-sections -g -fno-rtti -Wall -pedantic")

target_link_libraries(cde ${BDB_LIBRARIES} ${CLANG_LIBRARIES} ${LLVM_LIBRARIES} fileutil stdc++fs)

if (INCLUDE_TESTS)
  enable_testing()
  add_subdirectory(tests)
endif()
