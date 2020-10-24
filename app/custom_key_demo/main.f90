!> Example program demonstrating how a custom key-type 
!>  can be used.
!> See the `my_key_type` module for the definition of the
!>  custom key type.
!>
!> See README.md for an explanation
!>
program fhash_demo
  use fhash, only: fhash_tbl_t
  use my_key_type, only: string_t, key_string_t, key=>fhash_key
  implicit none
  
  type(fhash_tbl_t) :: tbl
  type(string_t) :: str1, str2
  integer :: val

  print *, '# fhash demo program: custom-key-demo'

  str1%s = 'Hello world'
  str2%s = 'Hello fhash'
  
  print *, 'Storing value 10 with key: "',str1%s,'"'
  call tbl%set(key([str1]), value=10)

  print *, 'Storing value 20 with key: "',str2%s,'"'
  call tbl%set(key([str2]), value=20)

  print *, 'Storing value 30 with key: ["',str1%s,'", "',str2%s,'"]'
  call tbl%set(key([str1,str2]), value=30)

  print *, 'Retrieving value with key: "',str1%s,'"'
  call tbl%get(key([str1]),val)
  print *, '   value = ',val

  print *, 'Retrieving value with key: "',str2%s,'"'
  call tbl%get(key([str2]),val)
  print *, '   value = ',val

  print *, 'Retrieving value with key: ["',str1%s,'", "',str2%s,'"]'
  call tbl%get(key([str1,str2]),val)
  print *, '   value = ',val

  print *

end program fhash_demo
