module test_tbl
  use iso_fortran_env, only: sp=>real32, dp=>real64, int32, int64
  use TestLite_suite, only : new_unittest, unittest_t, error_t, test_failed
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
        & new_unittest("fhash-tbl-intrinsics", test_fhash_intrinsics), &
        & new_unittest("fhash-tbl-intrinsic-pointers", test_fhash_intrinsic_ptrs), &
        & new_unittest("fhash-tbl-value-pointer", test_fhash_value_pointer), &
        & new_unittest("fhash-tbl-pointer-value", test_fhash_pointer_value), &
        & new_unittest("fhash-tbl-derived-type-value", test_fhash_derived_type_value), &
        & new_unittest("fhash-tbl-invalid-keys", test_fhash_invalid_keys), &
        & new_unittest("fhash-tbl-stats-empty", test_fhash_stats_empty), &
        & new_unittest("fhash-tbl-unset", test_fhash_unset), &
        & new_unittest("fhash-tbl-high-load", test_fhash_balanced_load) &
        ]
        
  end subroutine collect_tbl

  !>  Test intrinsic set and retrieve
  subroutine test_fhash_intrinsics(error)
    type(error_t), allocatable, intent(out) :: error

    type(fhash_tbl_t) :: tbl
    integer(int32) :: set_int32, get_int32
    integer(int64) :: set_int64, get_int64
    real(sp) :: set_float, get_float
    real(dp) :: set_double, get_double
    character(:), allocatable :: set_char, get_char
    logical :: set_bool, get_bool

    ! Set values
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

    ! Get values
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

  end subroutine test_fhash_intrinsics


  !>  Test intrinsic pointer set and retrieve
  subroutine test_fhash_intrinsic_ptrs(error)
    type(error_t), allocatable, intent(out) :: error

    type(fhash_tbl_t) :: tbl
    integer(int32), target :: set_int32
    integer(int64), target :: set_int64
    real(sp), target :: set_float
    real(dp), target :: set_double
    character(:), allocatable, target :: set_char
    logical, target :: set_bool

    integer(int32), pointer :: get_int32
    integer(int64), pointer :: get_int64
    real(sp), pointer :: get_float
    real(dp), pointer :: get_double
    character(:), pointer :: set_r, get_char
    logical, pointer :: get_bool
    
    ! Set pointers
    set_int32 =123
    call tbl%set_ptr(key('int32'),set_int32)

    set_int64 = 456
    call tbl%set_ptr(key('int64'),set_int64)

    set_float = 1.0_sp
    call tbl%set_ptr(key('float'),set_float)

    set_double = 2.0_dp
    call tbl%set_ptr(key('double'),set_double)

    set_char = 'Hello world'
    call tbl%set_ptr(key('char'),set_char)

    set_bool = .false.
    call tbl%set_ptr(key('bool'),set_bool)

    ! Get pointers
    call tbl%get_ptr(key('int32'),get_int32)
    if (.not.associated(get_int32,set_int32)) then
      call test_failed(error,'int32 pointer retrieved is not associated with variable set.')
      return
    end if

    call tbl%get_ptr(key('int64'),get_int64)
    if (.not.associated(get_int64,set_int64)) then
      call test_failed(error,'int64 pointer retrieved is not associated with variable set.')
      return
    end if

    call tbl%get_ptr(key('float'),get_float)
    if (.not.associated(get_float,set_float)) then
      call test_failed(error,'float pointer retrieved is not associated with variable set.')
      return
    end if

    call tbl%get_ptr(key('double'),get_double)
    if (.not.associated(get_double,set_double)) then
      call test_failed(error,'double pointer retrieved is not associated with variable set.')
      return
    end if

    call tbl%get_ptr(key('char'),get_char)
    if (.not.associated(get_char,set_char)) then
      call test_failed(error,'char pointer retrieved is not associated with variable set.')
      return
    end if

    call tbl%get_ptr(key('bool'),get_bool)
    if (.not.associated(get_bool,set_bool)) then
      call test_failed(error,'bool pointer retrieved is not associated with variable set.')
      return
    end if

  end subroutine test_fhash_intrinsic_ptrs


  !>  Store a value and retrieve a pointer
  subroutine test_fhash_value_pointer(error)
    type(error_t), allocatable, intent(out) :: error

    type(fhash_tbl_t) :: tbl
    integer :: stat
    character(:), pointer :: ptr

    call tbl%set(key('key'),'A string to store')

    call tbl%check_key(key('key'),stat)
    if (stat /= 0) then
      call test_failed(error,'Key check failed, error setting key-value.')
      return
    end if

    call tbl%get_ptr(key('key'),ptr)

    if (.not.associated(ptr)) then
      call test_failed(error,'Retrieved pointer is not associated.')
      return
    end if

    if (ptr /= 'A string to store') then
      call test_failed(error,'Retrieved pointer has the wrong value.')
      return
    end if

  end subroutine test_fhash_value_pointer


  !>  Store a pointer and retrieve a value
  subroutine test_fhash_pointer_value(error)
    type(error_t), allocatable, intent(out) :: error

    type(fhash_tbl_t) :: tbl
    integer :: stat
    character(:), allocatable :: set_char
    character(:), allocatable :: get_char

    set_char = 'A string to store'

    call tbl%set_ptr(key('key'),set_char)

    call tbl%check_key(key('key'),stat)
    if (stat /= 0) then
      call test_failed(error,'Key check failed, error setting key-value.')
      return
    end if

    ! Mutate value
    set_char(:) = 'An updated string'

    call tbl%get(key('key'),get_char,stat)

    if (stat /= 0 .OR. .not.allocated(get_char)) then
      call test_failed(error,'Retrieved pointer is not associated.')
      return
    end if

    if (get_char /= set_char) then
      call test_failed(error,'Retrieved pointer has the wrong value.')
      return
    end if

  end subroutine test_fhash_pointer_value


  !>  Test set and unset
  subroutine test_fhash_unset(error)
    use fhash_tbl, only: FHASH_KEY_NOT_FOUND
    type(error_t), allocatable, intent(out) :: error

    type(fhash_tbl_t) :: tbl
    integer :: var, stat

    call tbl%set(key('key'),'A string to store')

    call tbl%check_key(key('key'),stat)
    if (stat /= 0) then
      call test_failed(error,'Key check failed, error setting key-value.')
      return
    end if
    
    call tbl%unset(key('key'))

    call tbl%get(key('key'),var,stat)
    if (stat /= FHASH_KEY_NOT_FOUND) then
      print *, 'Stat = ',stat
      call test_failed(error,'Key unset failed, tbl%get: expecting stat = FHASH_KEY_NOT_FOUND')
      return
    end if

    call tbl%check_key(key('key'),stat)
    if (stat /= FHASH_KEY_NOT_FOUND) then
      print *, 'Stat = ',stat
      call test_failed(error,'Key unset failed, tbl%check_key: expecting stat = FHASH_KEY_NOT_FOUND')
      return
    end if

  end subroutine test_fhash_unset


  subroutine test_fhash_derived_type_value(error)
    type(error_t), allocatable, intent(out) :: error

    type string_t
      character(:), allocatable :: s
    end type string_t
    
    type(fhash_tbl_t) :: tbl
    type(string_t) :: str1, str2
    
    str1%s = 'Hello fhash'
    call tbl%set(key('key_1'), value=str1)
    
    call fhash_get_string(tbl,key('key_1'),str2,error)
    
    if (str1%s /= str2%s) then
      call test_failed(error,'Retrieved derived type does not match value set')
      return
    end if

    contains
    
    !> Custom getter for string_t type
    subroutine fhash_get_string(tbl,k,string,error)
      use fhash, only: fhash_key_t
      type(fhash_tbl_t), intent(in) :: tbl
      class(fhash_key_t), intent(in) :: k
      type(string_t), intent(out) :: string
      type(error_t), allocatable, intent(out) :: error
      
      integer :: stat
      class(*), allocatable :: data
      
      call tbl%get_raw(k,data,stat)
      
      if (stat /= 0) then
        call test_failed(error,'Error while trying to retrieve derived type')
        return
      end if 

      select type(d=>data)
      type is (string_t)
        string = d
      class default
        call test_failed(error,'Retrieved value type does not match expected derived type')
        return
      end select
      
    end subroutine fhash_get_string

  end subroutine test_fhash_derived_type_value


  !>  Try to retrieve invalid keys
  subroutine test_fhash_invalid_keys(error)
    use fhash_tbl, only: FHASH_EMPTY_TABLE, FHASH_KEY_NOT_FOUND, FHASH_FOUND_WRONG_TYPE
    type(error_t), allocatable, intent(out) :: error

    type(fhash_tbl_t) :: tbl
    integer :: var, stat

    call tbl%get(key('key1'),var,stat)

    if (stat /= FHASH_EMPTY_TABLE) then
      call test_failed(error,'Wrong stat value returned, expecting FHASH_EMPTY_TABLE.')
      return
    end if

    call tbl%set(key('key2'),'A string to store')

    call tbl%get(key('key3'),var,stat)

    if (stat /= FHASH_KEY_NOT_FOUND) then
      call test_failed(error,'Wrong stat value returned, expecting FHASH_KEY_NOT_FOUND.')
      return
    end if

    call tbl%get(key('key2'),var,stat)

    if (stat /= FHASH_FOUND_WRONG_TYPE) then
      print *, 'stat = ',stat
      call test_failed(error,'Wrong stat value returned, expecting FHASH_FOUND_WRONG_TYPE.')
      return
    end if

  end subroutine test_fhash_invalid_keys

  !>  Check stats for empty table
  subroutine test_fhash_stats_empty(error)
    type(error_t), allocatable, intent(out) :: error

    type(fhash_tbl_t) :: tbl
    integer :: num_buckets, num_items, num_collisions, max_depth

    call tbl%stats(num_buckets,num_items,num_collisions,max_depth)

    if (num_buckets /= 0) then
      print *, 'num_buckets: ', num_buckets
      call test_failed(error,'empty table returned non-zero for num_buckets')
      return
    end if

    if (num_items /= 0) then
      print *, 'num_items: ', num_items
      call test_failed(error,'empty table returned non-zero for num_items')
      return
    end if

    if (num_collisions /= 0) then
      print *, 'num_collisions: ', num_collisions
      call test_failed(error,'empty table returned non-zero for num_collisions')
      return
    end if

    if (max_depth > 0) then
      print *, 'max_depth: ', max_depth
      call test_failed(error,'empty table returned positive for max_depth')
      return
    end if

  end subroutine test_fhash_stats_empty


  !>  Store lots of values and check stats
  subroutine test_fhash_balanced_load(error)
    type(error_t), allocatable, intent(out) :: error

    type(fhash_tbl_t) :: tbl
    integer :: i, j, val, stat
    integer :: num_buckets, num_items, num_collisions, max_depth
    character(2) :: key_str

    call tbl%allocate(877)

    do i=0,25
      do j=0,25

        key_str = char(iachar('a')+i)//char(iachar('a')+j)
        call tbl%set(key(key_str),i*j)

      end do
    end do

    do i=0,25
      do j=0,25

        key_str = char(iachar('a')+i)//char(iachar('a')+j)
        call tbl%get(key(key_str),val,stat)

        if (stat /= 0) then
          call test_failed(error,'Error while retrieving value for key"'//key_str//'"')
          return
        end if

        if (val /= i*j) then
          call test_failed(error,'Incorrect value retrieved for key"'//key_str//'"')
          return
        end if

      end do
    end do

    call tbl%stats(num_buckets,num_items,num_collisions,max_depth)

    if (num_buckets /= 877) then
      print *, 'num_buckets: ', num_buckets
      call test_failed(error,'stats routine returned incorrect value for num_buckets')
      return
    end if

    if (num_items /= 26*26) then
      print *, 'num_items: ', num_items
      call test_failed(error,'stats routine returned incorrect value for num_items')
      return
    end if

    if (num_collisions > 200) then
      print *, 'num_collisions: ', num_collisions
      call test_failed(error,'stats routine returned a larger than expected value for num_collisions')
      return
    end if

    if (max_depth > 5) then
      print *, 'max_depth: ', max_depth
      call test_failed(error,'stats routine returned a larger than expected value for max_depth')
      return
    end if

  end subroutine test_fhash_balanced_load


end module test_tbl