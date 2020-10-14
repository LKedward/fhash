module fhash_tbl
  use iso_fortran_env, only: int32, int64, sp=>real32, dp=>real64
  use fhash_sll
  implicit none

  private
  public fhash_tbl_t

  !> This condition should be unreachable by the public interface
  integer, parameter, public :: FHASH_INTERNAL_ERROR = -4

  !> Error flag for operating on an unallocated table
  integer, parameter, public :: FHASH_EMPTY_TABLE = -3

   !> Error flag for when retrieved data-type does not 
  !>  match that expected by the invoked getter function 
  !>  (`get_int32`,`get_int63`,`get_float`,'get_double`,`get_char`)
  integer, parameter, public :: FHASH_FOUND_WRONG_TYPE = -2

  !> Error flag for when specified key is not found in the hash table
  integer, parameter, public :: FHASH_KEY_NOT_FOUND = -1

  !> Default allocation size
  integer, parameter :: FHASH_DEFAULT_ALLOCATION = 127

  type fhash_tbl_t

    type(fhash_node_t), allocatable :: buckets(:)

  contains

    procedure :: allocate => fhash_tbl_allocate
    procedure :: unset => fhash_tbl_unset

    procedure :: fhash_tbl_set_scalar
    generic :: set => fhash_tbl_set_scalar

    procedure :: fhash_tbl_set_scalar_ptr
    generic :: set_ptr => fhash_tbl_set_scalar_ptr

    procedure :: fhash_tbl_get_scalar
    generic :: get_raw => fhash_tbl_get_scalar

    procedure :: fhash_tbl_get_scalar_ptr
    generic :: get_raw_ptr => fhash_tbl_get_scalar_ptr

    procedure :: fhash_tbl_get_int32, fhash_tbl_get_int64
    procedure :: fhash_tbl_get_float, fhash_tbl_get_double
    procedure :: fhash_tbl_get_char,fhash_tbl_get_logical

    generic :: get => fhash_tbl_get_int32, fhash_tbl_get_int64
    generic :: get => fhash_tbl_get_float, fhash_tbl_get_double
    generic :: get => fhash_tbl_get_char, fhash_tbl_get_logical

    procedure :: fhash_tbl_get_int32_ptr, fhash_tbl_get_int64_ptr
    procedure :: fhash_tbl_get_float_ptr, fhash_tbl_get_double_ptr
    procedure :: fhash_tbl_get_char_ptr,fhash_tbl_get_logical_ptr

    generic :: get_ptr => fhash_tbl_get_int32_ptr, fhash_tbl_get_int64_ptr
    generic :: get_ptr => fhash_tbl_get_float_ptr, fhash_tbl_get_double_ptr
    generic :: get_ptr => fhash_tbl_get_char_ptr, fhash_tbl_get_logical_ptr

  end type fhash_tbl_t
  
contains

!> Allocate hash table
subroutine fhash_tbl_allocate(tbl,size)

  !> Table object to allocate
  class(fhash_tbl_t), intent(inout) :: tbl

  !> Number of buckets in hash table
  !> If ommited, `tbl` is allocated with `FHASH_DEFAULT_ALLOCATION`
  integer, intent(in), optional :: size

  if (present(size)) then
    allocate(tbl%buckets(size))
  else
    allocate(tbl%buckets(FHASH_DEFAULT_ALLOCATION))
  end if

end subroutine fhash_tbl_allocate


!> Unset a value in the table
!> 
subroutine fhash_tbl_unset(tbl,key,stat)

  !> Hash table object
  class(fhash_tbl_t), intent(inout) :: tbl

  !> Key to remove
  class(fhash_key_t), intent(in) :: key

  !> Status flag. Zero if successful.
  !> Unsuccessful: FHASH_EMPTY_TABLE | `FHASH_KEY_NOT_FOUND`
  integer, intent(out), optional :: stat

  integer :: index
  logical :: found

  stat = 0

  if (.not.allocated(tbl%buckets)) then
    stat = FHASH_EMPTY_TABLE
    return
  end if

  index = modulo(key%hash(),size(tbl%buckets)) + 1
  call sll_remove(tbl%buckets(index),key,found)

  if (present(stat)) stat = merge(0,FHASH_KEY_NOT_FOUND,found)

end subroutine fhash_tbl_unset


!> Set/update a polymorphic scalar value in the table
!>
!> `tbl` is allocated with default size if not already allocated
subroutine fhash_tbl_set_scalar(tbl,key,value,pointer)

  !> Hash table object
  class(fhash_tbl_t), intent(inout) :: tbl

  !> Key to set/update
  class(fhash_key_t), intent(in) :: key

  !> Value for key
  class(*), intent(in), target :: value

  !> If .true., store a pointer to value instead of copying
  logical, intent(in), optional :: pointer

  integer :: index
  logical :: set_ptr
  type(fhash_container_t) :: value_container

  if (.not.allocated(tbl%buckets)) call fhash_tbl_allocate(tbl)

  if (.not.present(pointer)) then
    set_ptr = .false.
  else
    set_ptr = pointer
  end if
  
  if (set_ptr) then
    value_container%scalar_ptr => value
  else
    value_container%scalar_data = value
  end if

  index = modulo(key%hash(),size(tbl%buckets)) + 1
  call sll_push_node(tbl%buckets(index),key,value_container)

end subroutine fhash_tbl_set_scalar


!> Get wrapper routine for generic 'set_ptr'
!>
!> `tbl` is allocated with default size if not already allocated
subroutine fhash_tbl_set_scalar_ptr(tbl,key,value)

  !> Hash table object
  class(fhash_tbl_t), intent(inout) :: tbl

  !> Key to set/update
  class(fhash_key_t), intent(in) :: key

  !> Value for key
  class(*), intent(in), target :: value

  call fhash_tbl_set_scalar(tbl,key,value,pointer=.true.)

end subroutine fhash_tbl_set_scalar_ptr


!> Check if key exists in table
subroutine fhash_tbl_check_key(tbl,key,stat)

  !> Hash table object
  class(fhash_tbl_t), intent(inout) :: tbl

  !> Key to retrieve
  class(fhash_key_t), intent(in) :: key

  !> Status flag. Zero if key is found.
  !> Unsuccessful: `FHASH_EMPTY_TABLE` | `FHASH_KEY_NOT_FOUND`
  integer, intent(out) :: stat

  integer :: index
  type(fhash_container_t), pointer :: data

  if (.not.allocated(tbl%buckets)) then
    stat = FHASH_EMPTY_TABLE
    return
  end if

  stat = 0

  index = modulo(key%hash(),size(tbl%buckets)) + 1

  call sll_find_in(tbl%buckets(index),key,data)

  if (associated(data)) then

      stat = merge(0,FHASH_INTERNAL_ERROR, &
                    allocated(data%scalar_data) .OR. &
                    associated(data%scalar_ptr))

      return

  else

    stat = FHASH_KEY_NOT_FOUND
    return

  end if

end subroutine fhash_tbl_check_key


!> Retrieve a polymorphic scalar value from the hash table
subroutine fhash_tbl_get_scalar(tbl,key,value,stat)

  !> Hash table object
  class(fhash_tbl_t), intent(inout) :: tbl

  !> Key to retrieve
  class(fhash_key_t), intent(in) :: key

  !> Copy of value retrieved for key
  class(*), intent(out), allocatable :: value

  !> Status flag. Zero if successful.
  !> Unsuccessful: `FHASH_EMPTY_TABLE` | `FHASH_KEY_NOT_FOUND`
  integer, intent(out), optional :: stat

  integer :: index
  type(fhash_container_t), pointer :: data

  if (.not.allocated(tbl%buckets)) then
    if (present(stat)) stat = FHASH_EMPTY_TABLE
    return
  end if

  if (present(stat)) stat = 0

  index = modulo(key%hash(),size(tbl%buckets)) + 1

  call sll_find_in(tbl%buckets(index),key,data)

  if (associated(data)) then

    if (allocated(data%scalar_data)) then
      
      value = data%scalar_data
      return

    elseif (associated(data%scalar_ptr)) then

      value = data%scalar_ptr
      return

    else

      if (present(stat)) stat = FHASH_INTERNAL_ERROR
      return

    end if

  else

    if (present(stat)) stat = FHASH_KEY_NOT_FOUND
    return

  end if

end subroutine fhash_tbl_get_scalar


!> Retrieve a polymorphic scalar pointer from the hash table
subroutine fhash_tbl_get_scalar_ptr(tbl,key,value,stat)

  !> Hash table object
  class(fhash_tbl_t), intent(inout) :: tbl

  !> Key to retrieve
  class(fhash_key_t), intent(in) :: key

  !> Pointer to value retrieved for key
  class(*), intent(out), pointer :: value

  !> Status flag. Zero if successful.
  !> Unsuccessful: `FHASH_EMPTY_TABLE` | `FHASH_KEY_NOT_FOUND`
  integer, intent(out), optional :: stat

  integer :: index
  type(fhash_container_t), pointer :: data

  if (.not.allocated(tbl%buckets)) then
    if (present(stat)) stat = FHASH_EMPTY_TABLE
    return
  end if

  if (present(stat)) stat = 0

  index = modulo(key%hash(),size(tbl%buckets)) + 1

  call sll_find_in(tbl%buckets(index),key,data)

  if (associated(data)) then

    if (allocated(data%scalar_data)) then
      
      value => data%scalar_data
      return

    elseif (associated(data%scalar_ptr)) then

      value => data%scalar_ptr
      return

    else

      if (present(stat)) stat = FHASH_INTERNAL_ERROR
      return

    end if

  else

    if (present(stat)) stat = FHASH_KEY_NOT_FOUND
    return

  end if

end subroutine fhash_tbl_get_scalar_ptr


!> Get wrapper to retrieve a scalar intrinsic type value
subroutine fhash_tbl_get_intrinsic_scalar(tbl,key,i32,i64,r32,r64,char,bool,stat)

  !> Hash table object
  class(fhash_tbl_t), intent(inout) :: tbl

  !> Key to retrieve
  class(fhash_key_t), intent(in) :: key

  !> Value to retrieve
  integer(int32), intent(out), optional :: i32
  integer(int64), intent(out), optional :: i64
  real(sp), intent(out), optional :: r32
  real(dp), intent(out), optional :: r64
  character(:), allocatable, intent(out), optional :: char
  logical, intent(out), optional :: bool

  !> Status flag. Zero if successful.
  !> Unsuccessful: `FHASH_EMPTY_TABLE` | 
  !>  `FHASH_FOUND_WRONG_TYPE` | `FHASH_KEY_NOT_FOUND`
  integer, intent(out), optional :: stat
  
  integer :: local_stat
  class(*), allocatable :: data

  call fhash_tbl_get_scalar(tbl,key,data,local_stat)
  if (local_stat /= 0) then
    if (present(stat)) stat = local_stat
    return
  end if

  select type(d=>data)

  type is (integer(int32))
    if (present(i32)) then
      i32 = d
      return
    else
      if (present(stat)) stat = FHASH_FOUND_WRONG_TYPE
      return
    end if

  type is (integer(int64))
    if (present(i64)) then
      i64 = d
      return
    else
      if (present(stat)) stat = FHASH_FOUND_WRONG_TYPE
      return
    end if

  type is (real(sp))
    if (present(r32)) then
      r32 = d
      return
    else
      if (present(stat)) stat = FHASH_FOUND_WRONG_TYPE
      return
    end if

  type is (real(dp))
    if (present(r64)) then
      r64 = d
      return
    else
      if (present(stat)) stat = FHASH_FOUND_WRONG_TYPE
      return
    end if

  type is (character(*))
    if (present(char)) then
      char = d
      return
    else
      if (present(stat)) stat = FHASH_FOUND_WRONG_TYPE
      return
    end if

  type is (logical)
    if (present(bool)) then
      bool = d
      return
    else
      if (present(stat)) stat = FHASH_FOUND_WRONG_TYPE
      return
    end if

  class default
    if (present(stat)) stat = FHASH_FOUND_WRONG_TYPE
  end select

end subroutine fhash_tbl_get_intrinsic_scalar


!> Get wrapper to retrieve a scalar intrinsic type pointer
subroutine fhash_tbl_get_intrinsic_scalar_ptr(tbl,key,i32,i64,r32,r64,char,bool,stat)

  !> Hash table object
  class(fhash_tbl_t), intent(inout) :: tbl

  !> Key to retrieve
  class(fhash_key_t), intent(in) :: key

  !> Value to retrieve
  integer(int32), pointer, intent(out), optional :: i32
  integer(int64), pointer, intent(out), optional :: i64
  real(sp), pointer, intent(out), optional :: r32
  real(dp), pointer, intent(out), optional :: r64
  character(:), pointer, intent(out), optional :: char
  logical, pointer, intent(out), optional :: bool

  !> Status flag. Zero if successful.
  !> Unsuccessful: `FHASH_EMPTY_TABLE` | 
  !>  `FHASH_FOUND_WRONG_TYPE` | `FHASH_KEY_NOT_FOUND`
  integer, intent(out), optional :: stat
  
  integer :: local_stat
  class(*), pointer :: data

  call fhash_tbl_get_scalar_ptr(tbl,key,data,local_stat)
  if (local_stat /= 0) then
    if (present(stat)) stat = local_stat
    return
  end if

  select type(d=>data)

  type is (integer(int32))
    if (present(i32)) then
      i32 => d
      return
    else
      if (present(stat)) stat = FHASH_FOUND_WRONG_TYPE
      return
    end if

  type is (integer(int64))
    if (present(i64)) then
      i64 => d
      return
    else
      if (present(stat)) stat = FHASH_FOUND_WRONG_TYPE
      return
    end if

  type is (real(sp))
    if (present(r32)) then
      r32 => d
      return
    else
      if (present(stat)) stat = FHASH_FOUND_WRONG_TYPE
      return
    end if

  type is (real(dp))
    if (present(r64)) then
      r64 => d
      return
    else
      if (present(stat)) stat = FHASH_FOUND_WRONG_TYPE
      return
    end if

  type is (character(*))
    if (present(char)) then
      char => d
      return
    else
      if (present(stat)) stat = FHASH_FOUND_WRONG_TYPE
      return
    end if

  type is (logical)
    if (present(bool)) then
      bool => d
      return
    else
      if (present(stat)) stat = FHASH_FOUND_WRONG_TYPE
      return
    end if

  class default
    if (present(stat)) stat = FHASH_FOUND_WRONG_TYPE
  end select

end subroutine fhash_tbl_get_intrinsic_scalar_ptr


!> Get wrapper to directly retrieve a scalar int32 value
subroutine fhash_tbl_get_int32(tbl,key,value,stat)
  class(fhash_tbl_t), intent(inout) :: tbl        !! Hash table object
  class(fhash_key_t), intent(in) :: key           !! Key to retrieve
  integer(int32), intent(out) :: value            !! Output value
  integer, intent(out), optional :: stat          !! Status flag. Zero if successful.

  call fhash_tbl_get_intrinsic_scalar(tbl,key,i32=value,stat=stat)

end subroutine fhash_tbl_get_int32


!> Get wrapper to directly retrieve a scalar int64 value
subroutine fhash_tbl_get_int64(tbl,key,value,stat)
  class(fhash_tbl_t), intent(inout) :: tbl        !! Hash table object
  class(fhash_key_t), intent(in) :: key           !! Key to retrieve
  integer(int64), intent(out) :: value            !! Output value
  integer, intent(out), optional :: stat          !! Status flag. Zero if successful.

  call fhash_tbl_get_intrinsic_scalar(tbl,key,i64=value,stat=stat)

end subroutine fhash_tbl_get_int64


!> Get wrapper to directly retrieve a scalar float value
subroutine fhash_tbl_get_float(tbl,key,value,stat)
  class(fhash_tbl_t), intent(inout) :: tbl        !! Hash table object
  class(fhash_key_t), intent(in) :: key           !! Key to retrieve
  real(sp), intent(out) :: value                  !! Output value
  integer, intent(out), optional :: stat          !! Status flag. Zero if successful.

  call fhash_tbl_get_intrinsic_scalar(tbl,key,r32=value,stat=stat)

end subroutine fhash_tbl_get_float


!> Get wrapper to directly retrieve a scalar double value
subroutine fhash_tbl_get_double(tbl,key,value,stat)
  class(fhash_tbl_t), intent(inout) :: tbl        !! Hash table object
  class(fhash_key_t), intent(in) :: key           !! Key to retrieve
  real(dp), intent(out) :: value                  !! Output value
  integer, intent(out), optional :: stat          !! Status flag. Zero if successful.

  call fhash_tbl_get_intrinsic_scalar(tbl,key,r64=value,stat=stat)

end subroutine fhash_tbl_get_double


!> Get wrapper to directly retrieve a scalar character value
subroutine fhash_tbl_get_char(tbl,key,value,stat)
  class(fhash_tbl_t), intent(inout) :: tbl        !! Hash table object
  class(fhash_key_t), intent(in) :: key           !! Key to retrieve
  character(:), allocatable, intent(out) :: value !! Output value
  integer, intent(out), optional :: stat          !! Status flag. Zero if successful.

  call fhash_tbl_get_intrinsic_scalar(tbl,key,char=value,stat=stat)

end subroutine fhash_tbl_get_char


!> Get wrapper to directly retrieve a scalar logical value
subroutine fhash_tbl_get_logical(tbl,key,value,stat)
  class(fhash_tbl_t), intent(inout) :: tbl        !! Hash table object
  class(fhash_key_t), intent(in) :: key           !! Key to retrieve
  logical, intent(out) :: value                   !! Output value
  integer, intent(out), optional :: stat          !! Status flag. Zero if successful.
  
  call fhash_tbl_get_intrinsic_scalar(tbl,key,bool=value,stat=stat)

end subroutine fhash_tbl_get_logical



!> Get wrapper to directly retrieve a scalar int32 value
subroutine fhash_tbl_get_int32_ptr(tbl,key,value,stat)
  class(fhash_tbl_t), intent(inout) :: tbl        !! Hash table object
  class(fhash_key_t), intent(in) :: key           !! Key to retrieve
  integer(int32), pointer, intent(out) :: value   !! Output value pointer
  integer, intent(out), optional :: stat          !! Status flag. Zero if successful.

  call fhash_tbl_get_intrinsic_scalar_ptr(tbl,key,i32=value,stat=stat)

end subroutine fhash_tbl_get_int32_ptr


!> Get wrapper to directly retrieve a scalar int64 value
subroutine fhash_tbl_get_int64_ptr(tbl,key,value,stat)
  class(fhash_tbl_t), intent(inout) :: tbl        !! Hash table object
  class(fhash_key_t), intent(in) :: key           !! Key to retrieve
  integer(int64), pointer, intent(out) :: value   !! Output value pointer
  integer, intent(out), optional :: stat          !! Status flag. Zero if successful.

  call fhash_tbl_get_intrinsic_scalar_ptr(tbl,key,i64=value,stat=stat)

end subroutine fhash_tbl_get_int64_ptr


!> Get wrapper to directly retrieve a scalar float value
subroutine fhash_tbl_get_float_ptr(tbl,key,value,stat)
  class(fhash_tbl_t), intent(inout) :: tbl        !! Hash table object
  class(fhash_key_t), intent(in) :: key           !! Key to retrieve
  real(sp), pointer, intent(out) :: value         !! Output value pointer
  integer, intent(out), optional :: stat          !! Status flag. Zero if successful.

  call fhash_tbl_get_intrinsic_scalar_ptr(tbl,key,r32=value,stat=stat)

end subroutine fhash_tbl_get_float_ptr


!> Get wrapper to directly retrieve a scalar double value
subroutine fhash_tbl_get_double_ptr(tbl,key,value,stat)
  class(fhash_tbl_t), intent(inout) :: tbl        !! Hash table object
  class(fhash_key_t), intent(in) :: key           !! Key to retrieve
  real(dp), pointer, intent(out) :: value         !! Output value pointer
  integer, intent(out), optional :: stat          !! Status flag. Zero if successful.

  call fhash_tbl_get_intrinsic_scalar_ptr(tbl,key,r64=value,stat=stat)

end subroutine fhash_tbl_get_double_ptr


!> Get wrapper to directly retrieve a scalar character value
subroutine fhash_tbl_get_char_ptr(tbl,key,value,stat)
  class(fhash_tbl_t), intent(inout) :: tbl        !! Hash table object
  class(fhash_key_t), intent(in) :: key           !! Key to retrieve
  character(:), pointer, intent(out) :: value     !! Output value pointer
  integer, intent(out), optional :: stat          !! Status flag. Zero if successful.

  call fhash_tbl_get_intrinsic_scalar_ptr(tbl,key,char=value,stat=stat)

end subroutine fhash_tbl_get_char_ptr


!> Get wrapper to directly retrieve a scalar logical value
subroutine fhash_tbl_get_logical_ptr(tbl,key,value,stat)
  class(fhash_tbl_t), intent(inout) :: tbl        !! Hash table object
  class(fhash_key_t), intent(in) :: key           !! Key to retrieve
  logical, pointer, intent(out) :: value          !! Output value pointer
  integer, intent(out), optional :: stat          !! Status flag. Zero if successful.
  
  call fhash_tbl_get_intrinsic_scalar_ptr(tbl,key,bool=value,stat=stat)

end subroutine fhash_tbl_get_logical_ptr


end module fhash_tbl