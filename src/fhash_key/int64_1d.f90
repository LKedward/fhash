!> Implements a concrete type for 1D int64 array hash keys
!>
module fhash_key_int64_1d
  use iso_fortran_env, only: int64
  use fhash_key_base, only: fhash_key_t
  use fhash_fnv, only: fnv_1a
  implicit none

  private
  public fhash_key_int64_1d_t
  public fhash_key

  !> Hash table key container
  type, extends(fhash_key_t) :: fhash_key_int64_1d_t
    private
    integer(int64), allocatable :: value(:)
  contains
    procedure, pass :: hash => key_hash_int64_1d  
    procedure, pass :: equals => key_equal_int64_1d
    procedure, pass :: to_string => key_int64_1d_to_string
    end type fhash_key_int64_1d_t

  interface fhash_key
    module procedure :: key_from_int64_1d
  end interface fhash_key

contains


  !> Check if two keys are equal
  pure function key_equal_int64_1d(key1,key2) result(keys_equal)
    class(fhash_key_int64_1d_t), intent(in) :: key1
    class(fhash_key_t), intent(in) :: key2
    logical :: keys_equal

    keys_equal = .false.

    select type(k2=>key2)
    type is (fhash_key_int64_1d_t)
      if (.not.(allocated(key1%value) .and. allocated(k2%value))) then
        return
      end if
      if (size(key1%value) /= size(k2%value)) then
        return
      end if
      if (all(key1%value == k2%value)) then
        keys_equal = .true.
        return
      end if
    end select
    
  end function key_equal_int64_1d


  !> Generate hash of key
  pure function key_hash_int64_1d(key) result(hash)
      class(fhash_key_int64_1d_t), intent(in) :: key
      integer(int64) :: hash

      hash = fnv_1a(key%value)

  end function key_hash_int64_1d


  !> Generate string representation of hash
  pure function key_int64_1d_to_string(key) result(str)
    class(fhash_key_int64_1d_t), intent(in) :: key
    character(:), allocatable :: str

    allocate(character(1024) :: str)
    write(str,*) key%value
    str = trim(str)

  end function key_int64_1d_to_string


  !> Create new key container from a scalar int64
  function key_from_int64_1d(source) result(key)
    integer(int64), intent(in) :: source(:)
    type(fhash_key_int64_1d_t) :: key

    key%value = source

  end function key_from_int64_1d

  
end module fhash_key_int64_1d