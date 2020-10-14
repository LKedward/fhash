!> Implements simple container type
!>  for polymorphic scalars and 1D arrays
module fhash_data_container
implicit none

  private
  public fhash_container_t
  public fhash_container, fhash_container_allocated

  !> Generic container for scalar and 1D data
  type fhash_container_t

    class(*), allocatable :: scalar_data
    class(*), pointer :: scalar_ptr => NULL()
    class(*), allocatable :: array1d_data(:) 
    class(*), pointer :: array1d_ptr(:) => NULL()

  contains

    procedure :: allocated => fhash_container_allocated

  end type fhash_container_t

  !> Create a fhash_container object from a polymorphic value
  interface fhash_container
    module procedure fhash_container_scalar
    module procedure fhash_container_1d
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
        container%scalar_data = value
      end if
    else
      container%scalar_data = value
    end if

  end function fhash_container_scalar


  !> Helper to initialise a polymorphic data container with 1D array
  function fhash_container_1d(value,pointer) result(container)
    
    !> Value to store
    class(*), intent(in), target :: value(:)

    !> If .true., store pointer to value instead of copying
    logical, intent(in), optional :: pointer

    type(fhash_container_t) :: container

    if (present(pointer)) then
      if (pointer) then
        container%array1d_ptr => value
      else
        allocate(container%array1d_data, source = value)
      end if
    else
      allocate(container%array1d_data, source = value)
    end if

  end function fhash_container_1d


  !> Helper to determine if container contains anything
  function fhash_container_allocated(container) result(alloc)
    class(fhash_container_t), intent(in) :: container
    logical :: alloc

    alloc = allocated(container%scalar_data) .OR. &
                associated(container%scalar_ptr) .OR. &
                allocated(container%array1d_data) .OR. &
                associated(container%array1d_ptr)

  end function fhash_container_allocated


end module fhash_data_container