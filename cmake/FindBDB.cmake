if (DEFINED BDB_ROOT)
  if (NOT DEFINED BDB_INCLUDE_DIR)
    set(BDB_INCLUDE_DIR "${BDB_ROOT}/include")
  endif()
endif()

find_path(BDB_INCLUDE_DIR db_cxx.h PATHS BDB_INCLUDE_DIR)
if (BDB_INCLUDE_DIR STREQUAL BDB_INCLUDE_DIR-NOTFOUND)
  message(FATAL_ERROR "Could not find BerkeleyDB includes. Probably you want specify BDB_INCLUDE_DIR/BDB_ROOT ?")
endif()

string(REGEX REPLACE "/include$" "/lib64" BDB_FLIB_DIR ${BDB_INCLUDE_DIR})
string(REGEX REPLACE "/include$" "/lib" BDB_LIB64_DIR ${BDB_INCLUDE_DIR})

find_library(BDB_LIBRARIES db_cxx PATHS ${BDB_LIB_DIR} ${BDB_FLIB_DIR} ${BDB_LIB64_DIR})

if (BDB_LIBRARIES STREQUAL BDB_LIBRARIES-NOTFOUND)
  message(FATAL_ERROR "Could not find BerkeleyDB libs. Probably you want specify BDB_LIB_DIR/BDB_ROOT ?")
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(BerkeleyDB DEFAULT_MSG BDB_LIBRARIES BDB_INCLUDE_DIR)
mark_as_advanced(BDB_INCLUDE_DIR BDB_LIBRARIES)
