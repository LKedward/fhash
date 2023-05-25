!> Example module demonstrating how a custom key-type 
!>  can be made.
!>
!> See README.md for an explanation
!>
module my_key_type
  use iso_fortran_env, only: int64
  use fhash_key_base, only: fhash_key_t
  use fhash_fnv, only: fnv_1a
  implicit none

  private
  public :: string_t, key_string_t
  public :: fhash_key

  !> The custom type for which we wish to create a key
  type :: string_t

    character(:), allocatable :: s

  end type string_t

  !> A key type container for our custom type
  type, extends(fhash_key_t) :: key_string_t

    type(string_t), allocatable :: value(:)

  contains

      procedure :: hash => key_hash_string_t
      procedure :: equals => key_equals_string_t
      procedure, pass :: to_string => key_to_string

  end type key_string_t

  !> Override the existing `fhash_key` interface to additionally
  !>  support our custom key type
  interface fhash_key
    module procedure :: key_from_string_t
  end interface fhash_key

contains

  !> Implements equality operator for custom key type.
  !> 
  !> NB. Keys of different types are always not equal.
  !>
  pure function key_equals_string_t(key1,key2) result(keys_equal)
    class(key_string_t), intent(in) :: key1
    class(fhash_key_t), intent(in) :: key2
    logical :: keys_equal

    integer :: i
    keys_equal = .false.

    select type(k2=>key2)
    type is (key_string_t)
      
      if (size(key1%value) /= size(k2%value)) return

      do i=1,size(key1%value)
        if (key1%value(i)%s /= k2%value(i)%s) then
          return
        end if
      end do
      
      keys_equal = .true.
      return

    end select

  end function key_equals_string_t


  !> Implements hashing of the custom key type.
  !>
  !> NB. Elementary hash function `fnv_1a` provides support for
  !>  default scalar characters and 32bit/64bit scalar and 1D integers
  !>
  pure function key_hash_string_t(key) result(hash)
    class(key_string_t), intent(in) :: key
    integer(int64) :: hash
    
    integer :: i

    do i=1,size(key%value)

      if (i == 1) then
        hash = fnv_1a(key%value(i)%s)
      else
        hash = fnv_1a(hash, key%value(i)%s)
      end if

    end do

  end function key_hash_string_t


  !> Generate string representation of hash
  pure function key_to_string(key) result(str)
    class(key_string_t), intent(in) :: key
    character(:), allocatable :: str

    integer :: i

    str = ''
    do i=1,size(key%value)
      str = str//','//key%value(i)%s
    end do

  end function key_to_string


  !> Helper function to create new key container from
  !>  some set of inputs
  function key_from_string_t(source) result(key)
    type(string_t), intent(in) :: source(:)
    type(key_string_t) :: key

    key%value = source

  end function key_from_string_t

  
end module my_key_type