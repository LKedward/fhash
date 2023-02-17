module fhash_tbl
  use iso_fortran_env, only: int32, int64, sp=>real32, dp=>real64
  use fhash_data_container, only: fhash_container
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
    procedure :: check_key => fhash_tbl_check_key
    procedure :: stats => fhash_tbl_stats
    
    procedure :: fhash_tbl_set_scalar
    generic :: set => fhash_tbl_set_scalar

    procedure :: fhash_tbl_set_scalar_ptr
    generic :: set_ptr => fhash_tbl_set_scalar_ptr

    procedure :: fhash_tbl_get_int32, fhash_tbl_get_int64
    procedure :: fhash_tbl_get_float, fhash_tbl_get_double
    procedure :: fhash_tbl_get_char,fhash_tbl_get_logical
    procedure :: fhash_tbl_get_data,fhash_tbl_get_raw

    generic :: get => fhash_tbl_get_int32, fhash_tbl_get_int64
    generic :: get => fhash_tbl_get_float, fhash_tbl_get_double
    generic :: get => fhash_tbl_get_char, fhash_tbl_get_logical
    generic :: get => fhash_tbl_get_data
    generic :: get_raw => fhash_tbl_get_raw

    procedure :: fhash_tbl_get_int32_ptr, fhash_tbl_get_int64_ptr
    procedure :: fhash_tbl_get_float_ptr, fhash_tbl_get_double_ptr
    procedure :: fhash_tbl_get_char_ptr,fhash_tbl_get_logical_ptr
    procedure :: fhash_tbl_get_raw_ptr

    generic :: get_ptr => fhash_tbl_get_int32_ptr, fhash_tbl_get_int64_ptr
    generic :: get_ptr => fhash_tbl_get_float_ptr, fhash_tbl_get_double_ptr
    generic :: get_ptr => fhash_tbl_get_char_ptr, fhash_tbl_get_logical_ptr
    generic :: get_raw_ptr => fhash_tbl_get_raw_ptr

    final :: fhash_tbl_cleanup

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


!> Finalizer for fhash_tbl_t
subroutine fhash_tbl_cleanup(tbl)
  
  !> Table object to allocate
  type(fhash_tbl_t), intent(inout) :: tbl

  integer :: i

  if (.not.allocated(tbl%buckets)) return

  do i=1,size(tbl%buckets)

    call sll_clean(tbl%buckets(i))

  end do

end subroutine fhash_tbl_cleanup


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

  if (present(stat)) stat = 0

  if (.not.allocated(tbl%buckets)) then
    if (present(stat)) stat = FHASH_EMPTY_TABLE
    return
  end if

  index = modulo(key%hash(),size(tbl%buckets,kind=int64)) + 1
  call sll_remove(tbl%buckets(index),key,found)

  if (present(stat)) stat = merge(0,FHASH_KEY_NOT_FOUND,found)

end subroutine fhash_tbl_unset


!> Check if key exists in table
subroutine fhash_tbl_check_key(tbl,key,stat)

  !> Hash table object
  class(fhash_tbl_t), intent(in) :: tbl

  !> Key to retrieve
  class(fhash_key_t), intent(in) :: key

  !> Status flag. Zero if key is found.
  !> Unsuccessful: `FHASH_EMPTY_TABLE` | `FHASH_KEY_NOT_FOUND`
  integer, intent(out) :: stat

  integer :: index
  logical :: found
  type(fhash_container_t), pointer :: data

  if (.not.allocated(tbl%buckets)) then
    stat = FHASH_EMPTY_TABLE
    return
  end if

  stat = 0

  index = modulo(key%hash(),size(tbl%buckets,kind=int64)) + 1

  call sll_find_in(tbl%buckets(index),key,data,found)

  stat = merge(0,FHASH_KEY_NOT_FOUND,found)

  return

end subroutine fhash_tbl_check_key


!> Get stats about the hash table
subroutine fhash_tbl_stats(tbl,num_buckets,num_items,num_collisions,max_depth)

  !> Hash table object
  class(fhash_tbl_t), intent(in) :: tbl

  !> Number of buckets allocated in table
  integer, intent(out), optional :: num_buckets

  !> Number of key-value pairs stored in table
  integer, intent(out), optional :: num_items

  !> Number of hash collisions
  integer, intent(out), optional :: num_collisions

  !> Maximum depth of bucket in table
  integer, intent(out), optional :: max_depth

  integer :: i, depth

  ! Initialise stats
  if (present(num_items)) num_items = 0
  if (present(num_collisions)) num_collisions = 0
  if (present(max_depth)) max_depth = 0
  if (present(num_buckets)) num_buckets = 0

  if (.not.allocated(tbl%buckets)) return

  if (present(num_buckets)) then
    num_buckets = size(tbl%buckets)
  end if

  do i=1,size(tbl%buckets)
    
    depth = node_depth(tbl%buckets(i))
    
    if (present(num_items)) num_items = num_items + depth
    
    if (present(num_collisions)) num_collisions = num_collisions + &
                                          merge(depth-1,0,depth > 1)

    if (present(max_depth)) max_depth = max(max_depth,depth)
    
  end do

end subroutine fhash_tbl_stats


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

  if (.not.allocated(tbl%buckets)) call fhash_tbl_allocate(tbl)

  index = modulo(key%hash(),size(tbl%buckets,kind=int64)) + 1

  call sll_push_node(tbl%buckets(index),key,value,pointer)

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



!> Retrieve data container from the hash table
subroutine fhash_tbl_get_data(tbl,key,data,stat)

  !> Hash table object
  class(fhash_tbl_t), intent(in) :: tbl

  !> Key to retrieve
  class(fhash_key_t), intent(in) :: key

  !> Copy of value retrieved for key
  type(fhash_container_t), pointer :: data

  !> Status flag. Zero if successful.
  !> Unsuccessful: `FHASH_EMPTY_TABLE` | `FHASH_KEY_NOT_FOUND`
  integer, intent(out), optional :: stat

  integer :: index
  logical :: found

  if (.not.allocated(tbl%buckets)) then
    if (present(stat)) stat = FHASH_EMPTY_TABLE
    return
  end if

  if (present(stat)) stat = 0

  index = modulo(key%hash(),size(tbl%buckets,kind=int64)) + 1

  call sll_find_in(tbl%buckets(index),key,data,found)

  if (.not.found) then

    if (present(stat)) stat = FHASH_KEY_NOT_FOUND
    return

  end if

end subroutine fhash_tbl_get_data



!> Get wrapper to retrieve a scalar intrinsic type value
subroutine fhash_tbl_get_intrinsic_scalar(tbl,key,i32,i64,r32,r64,char,raw,bool,stat)

  !> Hash table object
  class(fhash_tbl_t), intent(in) :: tbl

  !> Key to retrieve
  class(fhash_key_t), intent(in) :: key

  !> Value to retrieve
  integer(int32), intent(out), optional :: i32
  integer(int64), intent(out), optional :: i64
  real(sp), intent(out), optional :: r32
  real(dp), intent(out), optional :: r64
  character(:), allocatable, intent(out), optional :: char
  logical, intent(out), optional :: bool
  class(*), allocatable, intent(out), optional :: raw

  !> Status flag. Zero if successful.
  !> Unsuccessful: `FHASH_EMPTY_TABLE` | 
  !>  `FHASH_FOUND_WRONG_TYPE` | `FHASH_KEY_NOT_FOUND`
  integer, intent(out), optional :: stat
  
  logical :: type_match
  integer :: local_stat
  type(fhash_container_t), pointer :: data

  character(:), allocatable :: char_temp

  if (present(stat)) stat = 0

  call fhash_tbl_get_data(tbl,key,data,local_stat)

  if (local_stat /= 0) then
    if (present(stat)) stat = local_stat
    return
  end if

  if (present(char)) then      ! (Work-around for weird gfortran bug re char dummy)

    call data%get(i32,i64,r32,r64,char_temp,bool,raw,type_match)

    if (type_match) char = char_temp

  else

    call data%get(i32,i64,r32,r64,bool=bool,raw=raw,match=type_match)

  end if 

  if (.not.type_match) then
    if (present(stat)) stat = FHASH_FOUND_WRONG_TYPE
    return
  end if

end subroutine fhash_tbl_get_intrinsic_scalar


!> Get wrapper to retrieve a scalar intrinsic type pointer
subroutine fhash_tbl_get_intrinsic_scalar_ptr(tbl,key,i32,i64,r32,r64,char,bool,raw,stat)

  !> Hash table object
  class(fhash_tbl_t), intent(in) :: tbl

  !> Key to retrieve
  class(fhash_key_t), intent(in) :: key

  !> Value to retrieve
  integer(int32), pointer, intent(out), optional :: i32
  integer(int64), pointer, intent(out), optional :: i64
  real(sp), pointer, intent(out), optional :: r32
  real(dp), pointer, intent(out), optional :: r64
  character(:), pointer, intent(out), optional :: char
  logical, pointer, intent(out), optional :: bool
  class(*), pointer, intent(out), optional :: raw

  !> Status flag. Zero if successful.
  !> Unsuccessful: `FHASH_EMPTY_TABLE` | 
  !>  `FHASH_FOUND_WRONG_TYPE` | `FHASH_KEY_NOT_FOUND`
  integer, intent(out), optional :: stat
  
  logical :: type_match
  integer :: local_stat
  type(fhash_container_t), pointer :: data

  character(:), pointer :: char_temp

  if (present(stat)) stat = 0

  call fhash_tbl_get_data(tbl,key,data,local_stat)

  if (local_stat /= 0) then
    if (present(stat)) stat = local_stat
    return
  end if

  if (present(char)) then   ! (Work-around for weird gfortran bug re char dummy)

    call data%get_ptr(i32,i64,r32,r64,char_temp,bool,raw,type_match)

    if (type_match) char => char_temp

  else

    call data%get_ptr(i32,i64,r32,r64,bool=bool,raw=raw,match=type_match)

  end if 

  if (.not.type_match) then
    if (present(stat)) stat = FHASH_FOUND_WRONG_TYPE
    return
  end if

end subroutine fhash_tbl_get_intrinsic_scalar_ptr


!> Get wrapper to directly retrieve a scalar int32 value
subroutine fhash_tbl_get_int32(tbl,key,value,stat)
  class(fhash_tbl_t), intent(in) :: tbl        !! Hash table object
  class(fhash_key_t), intent(in) :: key           !! Key to retrieve
  integer(int32), intent(out) :: value            !! Output value
  integer, intent(out), optional :: stat          !! Status flag. Zero if successful.

  call fhash_tbl_get_intrinsic_scalar(tbl,key,i32=value,stat=stat)

end subroutine fhash_tbl_get_int32


!> Get wrapper to directly retrieve a scalar int64 value
subroutine fhash_tbl_get_int64(tbl,key,value,stat)
  class(fhash_tbl_t), intent(in) :: tbl        !! Hash table object
  class(fhash_key_t), intent(in) :: key           !! Key to retrieve
  integer(int64), intent(out) :: value            !! Output value
  integer, intent(out), optional :: stat          !! Status flag. Zero if successful.

  call fhash_tbl_get_intrinsic_scalar(tbl,key,i64=value,stat=stat)

end subroutine fhash_tbl_get_int64


!> Get wrapper to directly retrieve a scalar float value
subroutine fhash_tbl_get_float(tbl,key,value,stat)
  class(fhash_tbl_t), intent(in) :: tbl        !! Hash table object
  class(fhash_key_t), intent(in) :: key           !! Key to retrieve
  real(sp), intent(out) :: value                  !! Output value
  integer, intent(out), optional :: stat          !! Status flag. Zero if successful.

  call fhash_tbl_get_intrinsic_scalar(tbl,key,r32=value,stat=stat)

end subroutine fhash_tbl_get_float


!> Get wrapper to directly retrieve a scalar double value
subroutine fhash_tbl_get_double(tbl,key,value,stat)
  class(fhash_tbl_t), intent(in) :: tbl        !! Hash table object
  class(fhash_key_t), intent(in) :: key           !! Key to retrieve
  real(dp), intent(out) :: value                  !! Output value
  integer, intent(out), optional :: stat          !! Status flag. Zero if successful.

  call fhash_tbl_get_intrinsic_scalar(tbl,key,r64=value,stat=stat)

end subroutine fhash_tbl_get_double


!> Get wrapper to directly retrieve a scalar character value
subroutine fhash_tbl_get_char(tbl,key,value,stat)
  class(fhash_tbl_t), intent(in) :: tbl        !! Hash table object
  class(fhash_key_t), intent(in) :: key           !! Key to retrieve
  character(:), allocatable, intent(out) :: value !! Output value
  integer, intent(out), optional :: stat          !! Status flag. Zero if successful.

  call fhash_tbl_get_intrinsic_scalar(tbl,key,char=value,stat=stat)

end subroutine fhash_tbl_get_char


!> Get wrapper to directly retrieve a scalar logical value
subroutine fhash_tbl_get_logical(tbl,key,value,stat)
  class(fhash_tbl_t), intent(in) :: tbl        !! Hash table object
  class(fhash_key_t), intent(in) :: key           !! Key to retrieve
  logical, intent(out) :: value                   !! Output value
  integer, intent(out), optional :: stat          !! Status flag. Zero if successful.
  
  call fhash_tbl_get_intrinsic_scalar(tbl,key,bool=value,stat=stat)

end subroutine fhash_tbl_get_logical


!> Get wrapper to directly retrieve underlying polymorhpic scalar value
subroutine fhash_tbl_get_raw(tbl,key,value,stat)
  class(fhash_tbl_t), intent(in) :: tbl        !! Hash table object
  class(fhash_key_t), intent(in) :: key           !! Key to retrieve
  class(*), allocatable, intent(out) :: value     !! Output value
  integer, intent(out), optional :: stat          !! Status flag. Zero if successful.
  
  call fhash_tbl_get_intrinsic_scalar(tbl,key,raw=value,stat=stat)

end subroutine fhash_tbl_get_raw


!> Get wrapper to directly retrieve a scalar int32 value
subroutine fhash_tbl_get_int32_ptr(tbl,key,value,stat)
  class(fhash_tbl_t), intent(in) :: tbl        !! Hash table object
  class(fhash_key_t), intent(in) :: key           !! Key to retrieve
  integer(int32), pointer, intent(out) :: value   !! Output value pointer
  integer, intent(out), optional :: stat          !! Status flag. Zero if successful.

  call fhash_tbl_get_intrinsic_scalar_ptr(tbl,key,i32=value,stat=stat)

end subroutine fhash_tbl_get_int32_ptr


!> Get wrapper to directly retrieve a scalar int64 value
subroutine fhash_tbl_get_int64_ptr(tbl,key,value,stat)
  class(fhash_tbl_t), intent(in) :: tbl        !! Hash table object
  class(fhash_key_t), intent(in) :: key           !! Key to retrieve
  integer(int64), pointer, intent(out) :: value   !! Output value pointer
  integer, intent(out), optional :: stat          !! Status flag. Zero if successful.

  call fhash_tbl_get_intrinsic_scalar_ptr(tbl,key,i64=value,stat=stat)

end subroutine fhash_tbl_get_int64_ptr


!> Get wrapper to directly retrieve a scalar float value
subroutine fhash_tbl_get_float_ptr(tbl,key,value,stat)
  class(fhash_tbl_t), intent(in) :: tbl        !! Hash table object
  class(fhash_key_t), intent(in) :: key           !! Key to retrieve
  real(sp), pointer, intent(out) :: value         !! Output value pointer
  integer, intent(out), optional :: stat          !! Status flag. Zero if successful.

  call fhash_tbl_get_intrinsic_scalar_ptr(tbl,key,r32=value,stat=stat)

end subroutine fhash_tbl_get_float_ptr


!> Get wrapper to directly retrieve a scalar double value
subroutine fhash_tbl_get_double_ptr(tbl,key,value,stat)
  class(fhash_tbl_t), intent(in) :: tbl        !! Hash table object
  class(fhash_key_t), intent(in) :: key           !! Key to retrieve
  real(dp), pointer, intent(out) :: value         !! Output value pointer
  integer, intent(out), optional :: stat          !! Status flag. Zero if successful.

  call fhash_tbl_get_intrinsic_scalar_ptr(tbl,key,r64=value,stat=stat)

end subroutine fhash_tbl_get_double_ptr


!> Get wrapper to directly retrieve a scalar character value
subroutine fhash_tbl_get_char_ptr(tbl,key,value,stat)
  class(fhash_tbl_t), intent(in) :: tbl        !! Hash table object
  class(fhash_key_t), intent(in) :: key           !! Key to retrieve
  character(:), pointer, intent(out) :: value     !! Output value pointer
  integer, intent(out), optional :: stat          !! Status flag. Zero if successful.

  call fhash_tbl_get_intrinsic_scalar_ptr(tbl,key,char=value,stat=stat)

end subroutine fhash_tbl_get_char_ptr


!> Get wrapper to directly retrieve a scalar logical value
subroutine fhash_tbl_get_logical_ptr(tbl,key,value,stat)
  class(fhash_tbl_t), intent(in) :: tbl        !! Hash table object
  class(fhash_key_t), intent(in) :: key           !! Key to retrieve
  logical, pointer, intent(out) :: value          !! Output value pointer
  integer, intent(out), optional :: stat          !! Status flag. Zero if successful.
  
  call fhash_tbl_get_intrinsic_scalar_ptr(tbl,key,bool=value,stat=stat)

end subroutine fhash_tbl_get_logical_ptr


!> Get wrapper to directly retrieve underlying polymorhpic scalar value
subroutine fhash_tbl_get_raw_ptr(tbl,key,value,stat)
  class(fhash_tbl_t), intent(in) :: tbl        !! Hash table object
  class(fhash_key_t), intent(in) :: key           !! Key to retrieve
  class(*), pointer, intent(out) :: value                   !! Output value
  integer, intent(out), optional :: stat          !! Status flag. Zero if successful.
  
  call fhash_tbl_get_intrinsic_scalar_ptr(tbl,key,raw=value,stat=stat)

end subroutine fhash_tbl_get_raw_ptr


end module fhash_tbl