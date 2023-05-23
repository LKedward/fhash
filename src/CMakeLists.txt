set(FHASH_SOURCES fhash.f90 fhash_sll.f90 fhash_data_container.f90 fhash_tbl.f90 fhash_tbl_iter.f90 fhash_fnv.f90 fhash_key/base.f90 fhash_key/int32.f90 fhash_key/int64.f90 fhash_key/char.f90 fhash_key/int32_1d.f90 fhash_key/int64_1d.f90)
add_library(fhash SHARED  ${FHASH_SOURCES})
target_include_directories(fhash PUBLIC
  $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>
  $<INSTALL_INTERFACE:${Fortran_MODULE_DIRECTORY}>)
install(TARGETS fhash EXPORT fhash
  LIBRARY DESTINATION lib ${CMAKE_INSTALL_LIBDIR})
