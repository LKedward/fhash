module fhash_tbl_iter
  use fhash_tbl, only: fhash_tbl_t
  use fhash_key_base, only: fhash_key_t
  use fhash_data_container, only: fhash_container_t
  use fhash_sll
  implicit none

  private
  public fhash_iter_t

  !> Iterator type for iterating over hash table items
  type fhash_iter_t

    type(fhash_tbl_t), pointer :: tbl => NULL()

    integer :: bucket = 1
    integer :: depth = 1

  contains
    procedure :: next => fhash_iter_next
    procedure :: reset => fhash_iter_reset
  end type fhash_iter_t


  interface fhash_iter_t
    module procedure :: fhash_iter_init
  end interface fhash_iter_t

  contains

  !> Initialise fhash iterator
  function fhash_iter_init(tbl) result(iter)
    type(fhash_tbl_t), intent(in), target :: tbl
    type(fhash_iter_t) :: iter

    iter%tbl => tbl

  end function fhash_iter_init

  
  !> Return next item from iterator
  function fhash_iter_next(iter,key,data) result(found)
    class(fhash_iter_t), intent(inout) :: iter
    class(fhash_key_t), intent(out), allocatable :: key
    class(*), allocatable, intent(out) :: data
    logical :: found
    
    type(fhash_container_t), pointer :: data_container
    class(*), pointer :: data_out
    
    found = .false.

    if (.not.associated(iter%tbl)) return

    do while (.not.found)
      if (iter%bucket > size(iter%tbl%buckets)) return
      if (.not.allocated(iter%tbl%buckets(iter%bucket)%key)) then
        iter%bucket = iter%bucket + 1
        cycle
      end if
      call sll_get_at(iter%tbl%buckets(iter%bucket),iter%depth,key,data_container,found)
      if (iter%depth > node_depth(iter%tbl%buckets(iter%bucket))) then
        iter%bucket = iter%bucket + 1
        iter%depth = 1
      else
        iter%depth = iter%depth + 1
      end if
    end do

    if (found) then
      call data_container%get(raw=data)  ! Extract underlying polymorphic data
    end if

  end function fhash_iter_next


  !> Reset iterator to beginning
  subroutine fhash_iter_reset(iter)
    class(fhash_iter_t), intent(inout) :: iter

    iter%bucket = 1
    iter%depth = 1

  end subroutine fhash_iter_reset


end module fhash_tbl_iter