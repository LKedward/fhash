!> Example program demonstrating how to iterate over items in hash table
program fhash_demo
  use fhash, only: fhash_tbl_t, key=>fhash_key, fhash_iter_t, fhash_key_t
  implicit none

  type(fhash_tbl_t) :: tbl
  type(fhash_iter_t) :: iter
  class(fhash_key_t), allocatable :: ikey
  class(*), allocatable :: idata
  
  print *, '# fhash demo program: iter-demo'
  
  call tbl%set(key('my_key_1'), value=10)
  call tbl%set(key('my_key_2'), value=1.0)
  call tbl%set(key(123456), value='a string value')
  call tbl%set(key([1,2,3,4,5]), value=.false.)

  !> Iterate over all items in table
  iter = fhash_iter_t(tbl)
  do while(iter%next(ikey,idata))

    write(*,*) 'Key = "'//ikey%to_string()//'"'
    
    call print_polymorphic(idata)

  end do

! call iter%reset()  ! (Reset if iterator needed again)


contains

  !> Helper routine to print out polymorphic variable for intrinsics data types
  subroutine print_polymorphic(data)
    class(*), intent(in) :: data

    select type(d=>data)

    type is(integer)
      write(*,*) d
    
    type is(real)
      write(*,*) d

    type is(character(*))
      write(*,*) d

    type is(logical)
      write(*,*) d

    class default
      write(*,*) '[unknown data type, print not implemented]'

    end select

  end subroutine print_polymorphic

end program fhash_demo