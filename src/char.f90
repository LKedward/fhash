!> Implements a concrete type for scalar int32 hash keys
!>
module fhash_key_char
  use iso_fortran_env, only: int32, int64
  use fhash_key_base, only: fhash_key_t
  use fhash_fnv, only: fnv_1a
  implicit none

  private
  public fhash_key_char_t
  public fhash_key

  !> Hash table key container
  type, extends(fhash_key_t) :: fhash_key_char_t
    private
    character(:), allocatable :: value
  contains
    procedure, pass :: hash => key_hash_char  
    procedure, pass :: equals => key_equal_char
    procedure, pass :: to_string => key_char_to_string
  end type fhash_key_char_t

  interface fhash_key
    module procedure :: key_from_char
  end interface fhash_key
  
  contains


  !> Check if two keys are equal
  pure function key_equal_char(key1,key2) result(keys_equal)
    class(fhash_key_char_t), intent(in) :: key1
    class(fhash_key_t), intent(in) :: key2
    logical :: keys_equal

    keys_equal = .false.

    select type(k2=>key2)
    type is (fhash_key_char_t)
      if (allocated(key1%value) .and. allocated(k2%value)) then
        if (key1%value == k2%value) then
          keys_equal = .true.
          return
        end if
      end if
    end select
    
  end function key_equal_char


  !> Generate hash of key
  pure function key_hash_char(key) result(hash)
    class(fhash_key_char_t), intent(in) :: key
    integer(int64) :: hash

    hash = fnv_1a(key%value)

  end function key_hash_char


  !> Generate string representation of hash
   function key_char_to_string(key) result(str)
    class(fhash_key_char_t), intent(in) :: key
    character(:), allocatable :: str

    str = key%value

  end function key_char_to_string


  !> Create new key container from a scalar int32
  function key_from_char(source) result(key)
    character(*), intent(in) :: source
    type(fhash_key_char_t) :: key

    key%value = source

  end function key_from_char

  
end module fhash_key_char