add_library(TestLite TestLite.f90 TestLite_suite.f90 TestLite_error.f90)

add_library(tests test_container.f90 test_fnv.f90 test_key.f90 test_sll.f90 test_tbl.f90)

add_executable(main main.f90)
target_link_libraries(main PRIVATE fhash TestLite tests)

add_test(main main)

add_custom_target(all_tests ALL
  DEPENDS main)

add_custom_command(
     TARGET all_tests
     COMMENT "Run tests"
     POST_BUILD
     WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
     COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure -C $<CONFIGURATION>
)
