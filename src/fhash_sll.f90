!> Implements singly-linked list (sll) node with generic data container
!>
module fhash_sll
  use iso_fortran_env, only: int32, int64
  use fhash_key_base, only: fhash_key_t
  use fhash_data_container, only: fhash_container_t
  implicit none

  !> Node type for hash table singly linked list
  type fhash_node_t

    class(fhash_key_t), allocatable :: key
    type(fhash_container_t) :: value
    type(fhash_node_t), pointer :: next => NULL()

  end type fhash_node_t

contains

  !> Append node to SLL
  recursive subroutine sll_push_node(node,key,value,pointer)

    !> Node to which to add data
    type(fhash_node_t), intent(inout) :: node

    !> Key to add
    class(fhash_key_t), intent(in) :: key

    !> Value to add
    class(*), intent(in), target :: value

    !> Store only a point if .true.
    logical, intent(in), optional :: pointer


    if (allocated(node%key)) then
        
      if (node%key == key) then

        call sll_node_set(node,value,pointer)
        return

      end if

      if (.not.associated(node%next)) then
        allocate(node%next)
      end if

      call sll_push_node(node%next,key,value,pointer)
          
    else

      node%key = key
      call sll_node_set(node,value,pointer)

    end if

  end subroutine sll_push_node


  !> Set container value in node
  !>
  subroutine sll_node_set(node,value,pointer)

    !> Node to which to add data
    type(fhash_node_t), intent(inout) :: node

    !> Value to set
    class(*), intent(in), target :: value

    !> Store only a pointer if .true.
    logical, intent(in), optional :: pointer

    if (present(pointer)) then
      if (pointer) then
        node%value%scalar_ptr => value
        return
      end if
    end if

    if (allocated(node%value%scalar_data)) deallocate(node%value%scalar_data)
    allocate(node%value%scalar_data, source = value)

  end subroutine sll_node_set


  !> Search for a node with a specific key.
  !> Returns a pointer to the 'data' component of the corresponding node.
  !> Pointer is not associated if node cannot be found
  recursive subroutine sll_find_in(node,key,data,found)

    !> Node to search in
    type(fhash_node_t), intent(in), target :: node

    !> Key to look for
    class(fhash_key_t) :: key

    !> Pointer to value container if found.
    !> (Unassociated if the key is not found in node)
    type(fhash_container_t), pointer, intent(out) :: data

    logical, intent(out), optional :: found
    
    data => NULL()

    if (present(found)) found = .false.

    if (.not.allocated(node%key)) then

      return

    else if (node%key == key) then

      if (present(found)) found = .true.
      data => node%value
      return

    else if (associated(node%next)) then

      call sll_find_in(node%next,key,data,found) 
      
    end if

  end subroutine sll_find_in


  !> Return a node at a specific depth in the sll
  recursive subroutine sll_get_at(node,depth,key,data,found)

    !> Node to search in
    type(fhash_node_t), intent(in), target :: node

    !> Node depth to access
    integer, intent(in) :: depth

    !> Key of found item
    !>  (Unallocated if no node is found at specified depth)
    class(fhash_key_t), intent(out), allocatable :: key

    !> Pointer to value container if found.
    !> (Unassociated if no node is found at specified depth)
    type(fhash_container_t), pointer, intent(out) :: data

    logical, intent(out), optional :: found
    
    data => NULL()

    if (present(found)) found = .false.

    if (.not.allocated(node%key)) then

      return

    else if (depth == 1) then

      if (present(found)) found = .true.
      key = node%key
      data => node%value
      return

    else if (associated(node%next)) then
      
      call sll_get_at(node%next,depth-1,key,data,found) 
      
    end if

  end subroutine sll_get_at


  !> Search for a node with a specific key and remove
  recursive subroutine sll_remove(node,key,found,parent_node)

    !> Node to remove from
    type(fhash_node_t), intent(inout) :: node

    !> Key to remove
    class(fhash_key_t) :: key

    !> Indicates if the key was found in node and removed
    logical, optional, intent(out) :: found

    !> Used internally
    type(fhash_node_t), intent(inout), optional :: parent_node

    type(fhash_node_t), pointer :: next_temp

    if (present(found)) then
      found = .false.
    end if

    if (.not.allocated(node%key)) then

      return

    else if (node%key == key) then

      if (present(found)) then
        found = .true.
      end if

      if (.not.present(parent_node)) then
        ! This is the top-level node
        if (associated(node%next)) then
          ! Replace with next
          next_temp => node%next
          node = next_temp
          deallocate(next_temp)
          return
        else
          ! No children, just deallocate
          deallocate(node%key)
          return
        end if

      else
        ! Not top-level node
        if (associated(node%next)) then
          ! Join previous with next
          next_temp => node%next
          deallocate(parent_node%next)
          parent_node%next => next_temp
          return
        else
          ! No children, just deallocate
          deallocate(node%key)
          deallocate(parent_node%next)
          return
        end if
      end if

    else if (associated(node%next)) then
      ! Look further down
      call sll_remove(node%next,key,found,node) 
      
    end if

  end subroutine sll_remove


  !> Deallocate node components and those of its children
  recursive subroutine sll_clean(node)

    !> Node to search in
    type(fhash_node_t), intent(inout) :: node

    if (associated(node%next)) then

      call sll_clean(node%next)
      deallocate(node%next)
      
    end if

  end subroutine sll_clean


  !> Determine depth of SLL
  function node_depth(node) result(depth)

    !> Node to check depth
    type(fhash_node_t), intent(in), target :: node

    integer :: depth

    type(fhash_node_t), pointer :: current

    if (.not.allocated(node%key)) then

      depth = 0
      return

    else

      depth = 1
      current => node
      do while(associated(current%next))
        depth = depth + 1
        current => current%next
      end do

    end if

  end function node_depth

  
end module fhash_sll