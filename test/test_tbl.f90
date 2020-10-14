module test_tbl
  use iso_fortran_env, only: sp=>real32, dp=>real64, int32, int64
  use errortest_suite, only : new_unittest, unittest_t, error_t, test_failed
  use fhash, only: key=>fhash_key, fhash_tbl_t
  implicit none

  private
  public collect_tbl

  contains

  !> Collect all exported unit tests
  subroutine collect_tbl(testsuite)

    !> Collection of tests
    type(unittest_t), allocatable, intent(out) :: testsuite(:)
    
    testsuite = [ &
        & new_unittest("fhash-tbl-scalars", test_fhash_scalars) &
        ]
        
  end subroutine collect_tbl

  !>  Test scalar set and retrieve
  subroutine test_fhash_scalars(error)
    type(error_t), allocatable, intent(out) :: error

    type(fhash_tbl_t) :: tbl
    integer(int32) :: set_int32, get_int32
    integer(int64) :: set_int64, get_int64
    real(sp) :: set_float, get_float
    real(dp) :: set_double, get_double
    character(:), allocatable :: set_char, get_char
    logical :: set_bool, get_bool

    set_int32 =123
    call tbl%set(key('int32'),set_int32)

    set_int64 = 456
    call tbl%set(key('int64'),set_int64)

    set_float = 1.0_sp
    call tbl%set(key('float'),set_float)

    set_double = 2.0_dp
    call tbl%set(key('double'),set_double)

    set_char = 'Hello world'
    call tbl%set(key('char'),set_char)

    set_bool = .false.
    call tbl%set(key('bool'),set_bool)


    call tbl%get(key('int32'),get_int32)
    if (get_int32 /= set_int32) then
        call test_failed(error,'int32 value retrieved does not match value set.')
        return
    end if

    call tbl%get(key('int64'),get_int64)
    if (get_int64 /= set_int64) then
        call test_failed(error,'int64 value retrieved does not match value set.')
        return
    end if

    call tbl%get(key('float'),get_float)
    if (get_float /= set_float) then
        call test_failed(error,'float value retrieved does not match value set.')
        return
    end if

    call tbl%get(key('double'),get_double)
    if (get_double /= set_double) then
        call test_failed(error,'double value retrieved does not match value set.')
        return
    end if
    
    call tbl%get(key('char'),get_char)
    if (get_char /= set_char) then
        call test_failed(error,'char value retrieved does not match value set.')
        return
    end if

    call tbl%get(key('bool'),get_bool)
    if (get_bool .neqv. set_bool) then
        call test_failed(error,'logical value retrieved does not match value set.')
        return
    end if

  end subroutine test_fhash_scalars


  !>  Test scalar set and pointer retrieve
  subroutine test_fhash_scalar_ptrs(error)
    type(error_t), allocatable, intent(out) :: error

    type(fhash_tbl_t) :: tbl
    integer(int32) :: set_int32
    integer(int64) :: set_int64
    real(sp) :: set_float
    real(dp) :: set_double
    character(:), allocatable :: set_char
    logical :: set_bool

    integer(int32), pointer :: get_int32
    integer(int64), pointer :: get_int64
    real(sp), pointer :: get_float
    real(dp), pointer :: get_double
    character(:), pointer :: set_r, get_char
    logical, pointer :: get_bool

    set_int32 =123
    call tbl%set(key('int32'),set_int32)

    set_int64 = 456
    call tbl%set(key('int64'),set_int64)

    set_float = 1.0_sp
    call tbl%set(key('float'),set_float)

    set_double = 2.0_dp
    call tbl%set(key('double'),set_double)

    set_char = 'Hello world'
    call tbl%set(key('char'),set_char)

    set_bool = .false.
    call tbl%set(key('bool'),set_bool)


    ! call tbl%get_raw_ptr(key('int32'),get_int32)
    ! if (get_int32 /= set_int32) then
    !     call test_failed(error,'int32 value retrieved does not match value set.')
    !     return
    ! end if

    ! call tbl%get_ptr(key('int64'),get_int64)
    ! if (get_int64 /= set_int64) then
    !     call test_failed(error,'int64 value retrieved does not match value set.')
    !     return
    ! end if

    ! call tbl%get_ptr(key('float'),get_float)
    ! if (get_float /= set_float) then
    !     call test_failed(error,'float value retrieved does not match value set.')
    !     return
    ! end if

    ! call tbl%get_ptr(key('double'),get_double)
    ! if (get_double /= set_double) then
    !     call test_failed(error,'double value retrieved does not match value set.')
    !     return
    ! end if
    
    ! call tbl%get_ptr(key('char'),get_char)
    ! if (get_char /= set_char) then
    !     call test_failed(error,'char value retrieved does not match value set.')
    !     return
    ! end if

    ! call tbl%get_ptr(key('bool'),get_bool)
    ! if (get_bool .neqv. set_bool) then
    !     call test_failed(error,'logical value retrieved does not match value set.')
    !     return
    ! end if

  end subroutine test_fhash_scalar_ptrs

end module test_tbl