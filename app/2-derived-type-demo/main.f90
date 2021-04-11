!> Example program demonstrating how to store and retrieve
!>  custom derived types using `fhash`
program fhash_demo
  use fhash, only: fhash_tbl_t, key=>fhash_key, fhash_key_t
  implicit none

  type string_t
    character(:), allocatable :: s
  end type string_t
  
  type(fhash_tbl_t) :: tbl
  character(:), allocatable :: key_str
  type(string_t) :: str1, str2

  print *, '# fhash demo program: derived-type-demo'

  str1%s = 'Hello fhash'
  key_str = 'my_key'
  
  print *, 'Storing value "',str1%s,'" with key: "',key_str,'"'
  call tbl%set(key(key_str), value=str1)

  print *, 'Retrieving value with key: "',key_str,'"'
  call fhash_get_string(tbl,key(key_str),str2)
  print *, '   value = "',str2%s,'"'

  print *

  contains
  
  !> Define custom getter for string_t type
  subroutine fhash_get_string(tbl,k,string)
    type(fhash_tbl_t), intent(in) :: tbl
    class(fhash_key_t), intent(in) :: k
    type(string_t), intent(out) :: string
    
    integer :: stat
    class(*), allocatable :: data
    
    call tbl%get_raw(k,data,stat)
    
    if (stat /= 0) print *, 'error ', stat! Error handling: key not found

    select type(d=>data)
    type is (string_t)
      string = d
    class default
      ! Error handling: found wrong type
      print *, 'error'
    end select
    
  end subroutine fhash_get_string

end program fhash_demo