project("cde-tests")

find_package (Boost COMPONENTS system filesystem unit_test_framework REQUIRED)
include_directories(${Boost_INCLUDE_DIRS})
add_definitions(-DBOOST_TEST_DYN_LINK)

if (NOT WIN32)
  add_executable(test_external test_external.cc)
  target_link_libraries(test_external ${Boost_LIBRARIES})
  add_test(external test_external  -l all)
  set_tests_properties(external PROPERTIES TIMEOUT 1000)
endif()
