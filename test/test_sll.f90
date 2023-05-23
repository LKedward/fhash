module test_sll
  use iso_fortran_env, only: int32, int64
  use TestLite_suite, only : new_unittest, unittest_t, error_t, test_failed
  use fhash_sll
  use fhash_data_container
  use fhash_key_char
  use fhash_key_int32
  implicit none

  private
  public collect_sll

  contains

  !> Collect all exported unit tests
  subroutine collect_sll(testsuite)

    !> Collection of tests
    type(unittest_t), allocatable, intent(out) :: testsuite(:)
    
    testsuite = [ &
        & new_unittest("sll-set", test_sll_set), &
        & new_unittest("sll-get", test_sll_get), &
        & new_unittest("sll-get_at", test_sll_get_at), &
        & new_unittest("sll-update", test_sll_update), &
        & new_unittest("sll-remove", test_sll_remove) &
        ]
        
  end subroutine collect_sll

  !>  Test node set
  subroutine test_sll_set(error)
    type(error_t), allocatable, intent(out) :: error

    type(fhash_node_t) :: node

    if (allocated(node%key)) then
      call test_failed(error,'Key already allocated.')
      return
    end if

    if (node%value%allocated()) then
      call test_failed(error,'Value already allocated.')
      return
    end if

    call sll_push_node(node,fhash_key('key'),value=9)

    if (.not.allocated(node%key)) then
      call test_failed(error,'Key not allocated.')
      return
    end if

    if (.not.node%value%allocated()) then
      call test_failed(error,'Value not allocated.')
      return
    end if

    if (associated(node%next)) then
      call test_failed(error,'Next should not allocated.')
      return
    end if

    ! Set child nodes
    call sll_push_node(node,fhash_key('key2'),value=10)
    call sll_push_node(node,fhash_key('key3'),value=11)

    if (.not.associated(node%next) .OR. &
        .not.associated(node%next%next)) then
      call test_failed(error,'node%next not associated.')
      return
    end if

    if (node_depth(node) /= 3) then
      call test_failed(error,'Incorrect node depth, expecting 3.')
      return
    end if

    call sll_clean(node)

  end subroutine test_sll_set


  !> Get node data
  subroutine test_sll_get(error)
    type(error_t), allocatable, intent(out) :: error

    type(fhash_node_t) :: node

    ! Set value
    call sll_push_node(node,fhash_key('key'),value=int(9,int32))

    ! Get value
    call check_node_get_int32(error,node,'key',expect=9)
    if (allocated(error)) return

    ! Set child nodes
    call sll_push_node(node,fhash_key('key2'),value=10)
    call sll_push_node(node,fhash_key('key3'),value=11)
    
    ! Check child node get
    call check_node_get_int32(error,node,'key2',expect=10)
    if (allocated(error)) return

    call check_node_get_int32(error,node,'key3',expect=11)
    if (allocated(error)) return
    
    ! Check for non-existent node
    call check_node_get_int32(error,node,'key4',should_exist=.false.)
    if (allocated(error)) return

    call sll_clean(node)

  end subroutine test_sll_get


  !> Get node data at specific depth
  subroutine test_sll_get_at(error)
    type(error_t), allocatable, intent(out) :: error

    type(fhash_node_t) :: node
    type(fhash_container_t), pointer :: data
    class(*), allocatable :: data_raw
    integer(int32) :: depth, data_values(3)
    class(fhash_key_t), allocatable :: key_out
    logical :: found

    ! Set value
    data_values = [9_int32,10_int32,11_int32]
    do depth=1,3
      call sll_push_node(node,fhash_key(depth),value=data_values(depth))
    end do

    ! Get nodes
    do depth=1,3

      call sll_get_at(node, depth=depth, key=key_out, data=data, found=found)

      if (.not.found) then
        call test_failed(error,'Could not find node at specified depth')
        return
      end if

      call data%get(raw=data_raw)

      select type(d=>data_raw)

      type is(integer(int32))

        if (d /= data_values(depth)) then
          call test_failed(error,'Incorrect values returned from sll_get_at')
          return
        end if

      class default
        call test_failed(error,'Retrieved data is not int32')
        return
      end select

    end do

    call sll_clean(node)

  end subroutine test_sll_get_at


  !> Update node data
  subroutine test_sll_update(error)
    type(error_t), allocatable, intent(out) :: error

    type(fhash_node_t) :: node

    ! Set value
    call sll_push_node(node,fhash_key('key'),value=int(9,int32))
    call sll_push_node(node,fhash_key('key2'),value=10)
    call sll_push_node(node,fhash_key('key3'),value=11)

    ! Update value
    call sll_push_node(node,fhash_key('key'),value=int(1,int32))
    call sll_push_node(node,fhash_key('key2'),value=int(2,int32))
    call sll_push_node(node,fhash_key('key3'),value=int(3,int32))

    ! Check updated get value
    call check_node_get_int32(error,node,key='key',expect=1)
    if (allocated(error)) return

    call check_node_get_int32(error,node,key='key2',expect=2)
    if (allocated(error)) return

    call check_node_get_int32(error,node,key='key3',expect=3)
    if (allocated(error)) return

    call sll_clean(node)

  end subroutine test_sll_update


  !> Remove node data
  subroutine test_sll_remove(error)
    type(error_t), allocatable, intent(out) :: error

    type(fhash_node_t) :: node
    logical :: found

    ! Set value
    call sll_push_node(node,fhash_key('key'),value=int(9,int32))
    call sll_push_node(node,fhash_key('key2'),value=int(10,int32))
    call sll_push_node(node,fhash_key('key3'),value=int(11,int32))
    call sll_push_node(node,fhash_key('key4'),value=int(12,int32))

    ! Try to remove non-existent node
    call sll_remove(node,fhash_key('key5'),found)
    if (found) then
      call test_failed(error,'Non-existent node return found=true during removal')
      return
    end if

    ! Remove middle node
    call remove_and_check('key2')
    if (allocated(error)) return

    ! Check other nodes unaffected
    call check_node_get_int32(error,node,key='key',expect=9)
    if (allocated(error)) return

    call check_node_get_int32(error,node,key='key3',expect=11)
    if (allocated(error)) return

    call check_node_get_int32(error,node,key='key4',expect=12)
    if (allocated(error)) return

    ! Remove bottom node
    call remove_and_check('key4')
    if (allocated(error)) return

    ! Check other nodes unaffected
    call check_node_get_int32(error,node,key='key',expect=9)
    if (allocated(error)) return

    call check_node_get_int32(error,node,key='key3',expect=11)
    if (allocated(error)) return

    ! Remove top node
    call remove_and_check('key')
    if (allocated(error)) return

    ! Check other nodes unaffected
    call check_node_get_int32(error,node,key='key3',expect=11)
    if (allocated(error)) return

    ! Remove last node
    call remove_and_check('key3')
    if (allocated(error)) return

    if (node_depth(node) /= 0) then
      call test_failed(error,'Incorrect node depth, expecting 0.')
      return
    end if

    call sll_clean(node)

    contains

    !> Remove node and check for proper removal
    subroutine remove_and_check(key)
      character(*), intent(in) :: key

      type(fhash_container_t), pointer :: data
      logical :: found

      call sll_remove(node,fhash_key(key),found)

      if (.not.found) then
        call test_failed(error,'Node not found while removing (key="'//key//'")')
        return
      end if

      ! Try to get value
      call check_node_get_int32(error,node,key,should_exist=.false.)
      if (allocated(error)) then
        error%message = 'Node data still found, not removed (key="'//key//'")'
        return
      end if

    end subroutine remove_and_check

  end subroutine test_sll_remove


  !> Test helper to retrieve and test and int32 data value
  subroutine check_node_get_int32(error,node,key,expect,should_exist)
    type(error_t), allocatable, intent(out) :: error
    type(fhash_node_t) :: node
    character(*), intent(in) :: key
    integer(int32), intent(in), optional :: expect
    logical, intent(in), optional :: should_exist

    logical :: expect_exists, found
    type(fhash_container_t), pointer :: data

    if (present(should_exist)) then
      expect_exists = should_exist
    else
      expect_exists = .true.
    end if

    call sll_find_in(node,fhash_key(key),data,found)

    if (found .neqv. expect_exists) then
      call test_failed(error,'Node data not found for key "'//key//'"')
      return
    end if

    if (found .and. present(expect)) then
      call check_int32_data(error,data,expect)
      if (allocated(error)) then
        error%message = 'Error while retrieving key "'//key//'"' // new_line('a') // error%message
      end if
    end if

  end subroutine check_node_get_int32

  
  !> Test helper to check for type and value of int32 data
  subroutine check_int32_data(error,data,expect)
    type(error_t), allocatable, intent(out) :: error
    type(fhash_container_t), intent(in) :: data
    integer(int32), intent(in) :: expect

    character(500) :: message

    select type(v=>data%scalar_data)
    type is (integer(int32))
      if (v /= expect) then

        write(message,'(A,I0,A,A,I0,A)') 'Wrong value for int32 data, expecting "',expect,'"',&
                         ' but got "',v,'"'

        call test_failed(error,trim(message))
        return

      end if
    class default
      call test_failed(error,'Wrong type for retrieved data.')
      return
    end select

  end subroutine check_int32_data

end module test_sll