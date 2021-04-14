module test_container
  use iso_fortran_env, only: int32, int64
  use TestLite_suite, only : new_unittest, unittest_t, error_t, test_failed
  use fhash_data_container
  implicit none

  private
  public collect_container

  contains

  !> Collect all exported unit tests
  subroutine collect_container(testsuite)

    !> Collection of tests
    type(unittest_t), allocatable, intent(out) :: testsuite(:)
    
    testsuite = [ &
        & new_unittest("container-scalar", test_container_scalar), &
        & new_unittest("container-scalar-ptr", test_container_scalar_ptr) &
        ]
        
  end subroutine collect_container


  !> Set & update scalar data
  subroutine test_container_scalar(error)
    type(error_t), allocatable, intent(out) :: error

    type(fhash_container_t) :: container

    ! Set value
    container = fhash_container(int(9,int32))

    if (.not.allocated(container%scalar_data)) then
      call test_failed(error,'Scalar_data not allocated.')
      return
    end if

    select type(v=>container%scalar_data)
    type is (integer(int32))
      if (v /= 9) then
        call test_failed(error,'Wrong value for container%scalar_data.')
        return
      end if
    class default
      call test_failed(error,'Wrong data type for container%scalar_data.')
      return
    end select

  end subroutine test_container_scalar


  !> Set & update scalar pointer
  subroutine test_container_scalar_ptr(error)
    type(error_t), allocatable, intent(out) :: error

    integer(int32), target :: my_int, new_int
    type(fhash_container_t) :: container

    my_int = 9

    ! Set value
    container = fhash_container(my_int,pointer=.true.)

    if (.not.associated(container%scalar_ptr)) then
      call test_failed(error,'Scalar_ptr not associated.')
      return
    end if

    my_int = 10

    select type(v=>container%scalar_ptr)
    type is (integer(int32))
      if (v /= my_int) then
        call test_failed(error,'Wrong value for container%scalar_ptr.')
        return
      end if
    class default
      call test_failed(error,'Wrong data type for container%scalar_ptr.')
      return
    end select
    
  end subroutine test_container_scalar_ptr

end module test_container