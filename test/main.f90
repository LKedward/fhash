program test
  use iso_fortran_env, only: int32, int64
  use TestLite, only: testsuite_t, new_testsuite, run_tests
  use TestLite_suite
  use TestLite_error
  use test_fnv, only: collect_fnv
  use test_key, only: collect_key
  use test_container, only: collect_container
  use test_sll, only: collect_sll
  use test_tbl, only: collect_tbl
  use test_tbl_iter, only: collect_tbl_iter
  implicit none

  type(testsuite_t), allocatable :: testsuite(:)

  testsuite = [ &
      & new_testsuite("fhash_fnv", collect_fnv), &
      & new_testsuite("fhash_key", collect_key), &
      & new_testsuite("fhash_container", collect_container), &
      & new_testsuite("fhash_sll", collect_sll), &
      & new_testsuite("fhash_tbl", collect_tbl), &
      & new_testsuite("fhash_tbl_iter", collect_tbl_iter) &
  ]

  call run_tests(testsuite)

end program test