!> Implements an abstract type for hash keys
!>
module fhash_key_base
  use iso_fortran_env, only: int32, int64
  implicit none

  private
  public fhash_key_t

  !> Abstract base type for defining hash keys
  type, abstract :: fhash_key_t
  contains
    procedure(hash_proc), deferred :: hash
    procedure(equality_proc), deferred :: equals
    procedure(to_string_proc), deferred :: to_string
    generic, public :: operator(==) => equals
  end type fhash_key_t

  abstract interface

    pure function equality_proc(key1,key2) result(keys_equal)
      import 
      class(fhash_key_t), intent(in) :: key1
      class(fhash_key_t), intent(in) :: key2
      logical :: keys_equal
    end function equality_proc

    pure function hash_proc(key) result(hash)
      import
      class(fhash_key_t), intent(in) :: key
      integer(int64) :: hash
    end function hash_proc

     function to_string_proc(key) result(str)
      import
      class(fhash_key_t), intent(in) :: key
      character(:), allocatable :: str
    end function to_string_proc

  end interface

end module fhash_key_base