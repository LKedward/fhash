module test_tbl_iter
    use iso_fortran_env, only: sp=>real32, dp=>real64, int32, int64
    use TestLite_suite, only : new_unittest, unittest_t, error_t, test_failed
    use fhash, only: fhash_key_t, key=>fhash_key, fhash_tbl_t, fhash_iter_t
    implicit none
  
    private
    public collect_tbl_iter
  
    contains
  
    !> Collect all exported unit tests
    subroutine collect_tbl_iter(testsuite)
  
      !> Collection of tests
      type(unittest_t), allocatable, intent(out) :: testsuite(:)
      
      testsuite = [ &
          & new_unittest("fhash-iter-intrinsics", test_iter_intrinsics) &
          ]
          
    end subroutine collect_tbl_iter
  
    !>  Test intrinsic set and retrieve
    subroutine test_iter_intrinsics(error)
      type(error_t), allocatable, intent(out) :: error
  
      type(fhash_tbl_t) :: tbl
      integer(int32) :: set_int32, get_int32
      integer(int64) :: set_int64, get_int64
      real(sp) :: set_float, get_float
      real(dp) :: set_double, get_double
      character(:), allocatable :: set_char, get_char
      logical :: set_bool, get_bool
      
      integer :: count
      type(fhash_iter_t) :: iter
      class(fhash_key_t), allocatable :: ikey
      class(*), allocatable :: idata

      ! Set values
      call tbl%set(key('int32'),123_int32)
      call tbl%set(key('int64'),456_int64)
      call tbl%set(key('float'),1.0_sp)
      call tbl%set(key('double'),2.0_dp)
      call tbl%set(key('char'),'Hello world')
      call tbl%set(key('bool'),.false.)
      
      ! Iterate over stored keys and check values
      count = 0
      iter = fhash_iter_t(tbl)
      do while(iter%next(ikey,idata))

        count = count + 1

        select type(d=>idata)
        type is(integer(int32))
          if (ikey%to_string() /= 'int32') then
            call test_failed(error,'int32 key has incorrect value')
            return
          end if
          if (d /= 123_int32) then
            call test_failed(error,'int32 data has incorrect value')
            return
          end if

        type is(integer(int64))
          if (ikey%to_string() /= 'int64') then
            call test_failed(error,'int64 key has incorrect value')
            return
          end if
          if (d /= 456_int64) then
            call test_failed(error,'int64 data has incorrect value')
            return
          end if

        type is(real(sp))
          if (ikey%to_string() /= 'float') then
            call test_failed(error,'float key has incorrect value')
            return
          end if
          if (d /= 1.0_sp) then
            call test_failed(error,'float data has incorrect value')
            return
          end if

        type is(real(dp))
          if (ikey%to_string() /= 'double') then
            call test_failed(error,'double key has incorrect value')
            return
          end if
          if (d /= 2.0_dp) then
            call test_failed(error,'double data has incorrect value')
            return
          end if

        type is(character(*))
          if (ikey%to_string() /= 'char') then
            call test_failed(error,'char key has incorrect value')
            return
          end if
          if (d /= 'Hello world') then
            call test_failed(error,'char data has incorrect value')
            return
          end if

        type is(logical)
          if (ikey%to_string() /= 'bool') then
            call test_failed(error,'bool key has incorrect value')
            return
          end if
          if (d) then
            call test_failed(error,'bool data has incorrect value')
            return
          end if
          
        end select

      end do

      if (count /= 6) then
        call test_failed(error,'incorrect number of items iteracted over')
        return
      end if
  
    end subroutine test_iter_intrinsics
  
  end module test_tbl_iter