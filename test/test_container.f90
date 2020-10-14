module test_container
  use iso_fortran_env, only: int32, int64
  use errortest_suite, only : new_unittest, unittest_t, error_t, test_failed
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
        & new_unittest("container-scalar-ptr", test_container_scalar_ptr), &
        & new_unittest("container-1d", test_container_1d), &
        & new_unittest("container-1d-ptr", test_container_1d_ptr) &
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

    if (.not.associated(container%scalar_ptr,my_int)) then
      call test_failed(error,'Wrong association for container%scalar_ptr.')
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


  !> Set & update 1D data
  subroutine test_container_1d(error)
    type(error_t), allocatable, intent(out) :: error

    integer(int32) :: my_data(6)
    type(fhash_container_t) :: container

    my_data = [0,1,2,3,4,5]

    ! Set value
    container = fhash_container(my_data)

    if (.not.allocated(container%array1d_data)) then
      call test_failed(error,'array1d_data not allocated.')
      return
    end if

    select type(v=>container%array1d_data)
    type is (integer(int32))
      if (any(v /= my_data)) then
        call test_failed(error,'Wrong value for container%array1d_data.')
        return
      end if
    class default
      call test_failed(error,'Wrong data type for container%array1d_data.')
      return
    end select

  end subroutine test_container_1d

  
  !> Set & update 1D pointer
  subroutine test_container_1d_ptr(error)
    type(error_t), allocatable, intent(out) :: error

    integer(int32), target :: my_data(6), new_data(2)
    integer(int32), pointer :: check_ptr(:)
    type(fhash_container_t) :: container
    
    my_data = [0,1,2,3,4,5]

    ! Set value
    container = fhash_container(my_data, pointer=.true.)

    if (.not.associated(container%array1d_ptr)) then
      call test_failed(error,'array1d_ptr not associated.')
      return
    end if

    my_data = [5,4,3,2,1,0]

    select type(v=>container%array1d_ptr)
    type is (integer(int32))

      check_ptr => v
      if (.not.associated(v,my_data)) then
        call test_failed(error,'Wrong association for array1d_ptr.')
        return
      end if

      if (any(v /= my_data)) then
        call test_failed(error,'Wrong value for container%array1d_ptr.')
        return
      end if

    class default
      call test_failed(error,'Wrong data type for container%array1d_ptr.')
      return
    end select

  end subroutine test_container_1d_ptr

end module test_container