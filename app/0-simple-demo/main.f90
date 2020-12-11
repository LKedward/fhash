!> Example program demonstrating basic set/get usage
!>  for different key/value types
program fhash_demo
  use fhash, only: fhash_tbl_t, key=>fhash_key
  implicit none

  type(fhash_tbl_t) :: tbl
  integer :: i
  real :: r
  character(:), allocatable :: char
  logical :: bool
  
  print *, '# fhash demo program: simple-demo'
  
  call tbl%set(key('my_key_1'), value=10)
  call tbl%set(key('my_key_2'), value=1.0)
  call tbl%set(key(123456), value='a string value')
  call tbl%set(key([1,2,3,4,5]), value=.false.)
  
  call tbl%get(key('my_key_1'),i)
  call tbl%get(key('my_key_2'),r)
  call tbl%get(key(123456),char)
  call tbl%get(key([1,2,3,4,5]),bool)
  
  print *, 'Key = "my_key_1"     Value = ',i
  print *, 'Key = "my_key_2"     Value = ',r
  print *, 'Key = 123456         Value = "',char,'"'
  print *, 'Key = [1,2,3,4,5]    Value = ', bool
  print *

end program fhash_demo