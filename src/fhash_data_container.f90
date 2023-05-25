!> Implements simple container type
!>  for polymorphic scalars and 1D arrays
module fhash_data_container
  use iso_fortran_env, only: sp=>real32, dp=>real64, int32, int64
  implicit none

  private
  public fhash_container_t
  public fhash_container

  !> Generic container for scalar and 1D data
  type fhash_container_t

    class(*), allocatable :: scalar_data
    class(*), pointer :: scalar_ptr => NULL()

  contains

    procedure :: allocated => fhash_container_allocated
    procedure :: get => fhash_container_get_scalar
    procedure :: get_ptr => fhash_container_get_scalar_ptr

  end type fhash_container_t

  !> Create a fhash_container object from a polymorphic value
  interface fhash_container
    module procedure fhash_container_scalar
  end interface fhash_container

contains

  !> Helper to initialise a polymorphic data container with scalar
  function fhash_container_scalar(value,pointer) result(container)

    !> Value to store
    class(*), intent(in), target :: value

    !> If .true., store pointer to value instead of copying
    logical, intent(in), optional :: pointer

    type(fhash_container_t) :: container

    if (present(pointer)) then
      if (pointer) then
        container%scalar_ptr => value
      else
        if (allocated(container%scalar_data)) deallocate(container%scalar_data)
        allocate(container%scalar_data, source = value)
      end if
    else
      if (allocated(container%scalar_data)) deallocate(container%scalar_data)
      allocate(container%scalar_data, source = value)
    end if

  end function fhash_container_scalar


  !> Helper to determine if container contains anything
  function fhash_container_allocated(container) result(alloc)
    class(fhash_container_t), intent(in) :: container
    logical :: alloc

    alloc = allocated(container%scalar_data) .OR. &
                associated(container%scalar_ptr)

  end function fhash_container_allocated


  !> Helper to return container value as intrinsic type
  subroutine fhash_container_get_scalar(container,i32,i64,r32,r64,char,bool,raw,match,type_string)
    class(fhash_container_t), intent(in), target :: container
    integer(int32), intent(out), optional :: i32
    integer(int64), intent(out), optional :: i64
    real(sp), intent(out), optional :: r32
    real(dp), intent(out), optional :: r64
    character(:), allocatable, intent(out), optional :: char
    logical, intent(out), optional :: bool
    class(*), allocatable, intent(out), optional :: raw
    logical, intent(out), optional :: match
    character(:), allocatable, intent(out), optional :: type_string
    
    class(*), pointer :: data

    if (present(match)) match = .false.

    if (.not.container%allocated()) return

    if (allocated(container%scalar_data)) then
      data => container%scalar_data
    else
      data => container%scalar_ptr
    end if

    if (present(raw)) then
      if (present(match)) match = .true.
      allocate(raw, source=data)
    end if

    select type(d=>data)
    type is(integer(int32))
      if (present(type_string)) type_string = 'integer32'
      if (present(i32)) then
        if (present(match)) match = .true.
        i32 = d
        return
      end if

    type is (integer(int64))
      if (present(type_string)) type_string = 'integer64'
      if (present(i64)) then
        if (present(match)) match = .true.
        i64 = d
        return
      end if
  
    type is (real(sp))
      if (present(type_string)) type_string = 'real32'
      if (present(r32)) then
        if (present(match)) match = .true.
        r32 = d
        return
      end if
  
    type is (real(dp))
      if (present(type_string)) type_string = 'real64'
      if (present(r64)) then
        if (present(match)) match = .true.
        r64 = d
        return
      end if
  
    type is (character(*))
      if (present(type_string)) type_string = 'character*'
      if (present(char)) then
        if (present(match)) match = .true.
        char = d
        return
      end if
  
    type is (logical)
      if (present(type_string)) type_string = 'logical'
      if (present(bool)) then
        if (present(match)) match = .true.
        bool = d
        return
      end if
      
    class default
      if (present(type_string)) type_string = 'unknown'

    end select

  end subroutine fhash_container_get_scalar


  !> Helper to return pointer to container value as intrinsic type
  subroutine fhash_container_get_scalar_ptr(container,i32,i64,r32,r64,char,bool,raw,match,type_string)
    class(fhash_container_t), intent(in), target :: container
    integer(int32), pointer, intent(out), optional :: i32
    integer(int64), pointer, intent(out), optional :: i64
    real(sp), pointer, intent(out), optional :: r32
    real(dp), pointer, intent(out), optional :: r64
    character(:), pointer, intent(out), optional :: char
    logical, pointer, intent(out), optional :: bool
    class(*), pointer, intent(out), optional :: raw
    logical, intent(out), optional :: match
    character(:), allocatable, intent(out), optional :: type_string
    
    class(*), pointer :: data

    if (present(match)) match = .false.

    if (.not.container%allocated()) return

    if (allocated(container%scalar_data)) then
      data => container%scalar_data
    else
      data => container%scalar_ptr
    end if

    if (present(raw)) then
      if (present(match)) match = .true.
      raw => data
    end if

    select type(d=>data)
    type is(integer(int32))
      if (present(i32)) then
        if (present(match)) match = .true.
        if (present(type_string)) type_string = 'integer32'
        i32 => d
        return
      end if

    type is (integer(int64))
      if (present(i64)) then
        if (present(match)) match = .true.
        if (present(type_string)) type_string = 'integer64'
        i64 => d
        return
      end if
  
    type is (real(sp))
      if (present(r32)) then
        if (present(match)) match = .true.
        if (present(type_string)) type_string = 'real32'
        r32 => d
        return
      end if
  
    type is (real(dp))
      if (present(r64)) then
        if (present(match)) match = .true.
        if (present(type_string)) type_string = 'real64'
        r64 => d
        return
      end if
  
    type is (character(*))
      if (present(char)) then
        if (present(match)) match = .true.
        if (present(type_string)) type_string = 'character*'
        char => d
        return
      end if
  
    type is (logical)
      if (present(bool)) then
        if (present(match)) match = .true.
        if (present(type_string)) type_string = 'logical'
        bool => d
        return
      end if
        
    class default
      if (present(type_string)) type_string = 'unknown'

    end select

  end subroutine fhash_container_get_scalar_ptr
  

end module fhash_data_container