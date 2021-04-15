!> A module for Fowler–Noll–Vo (FNV) hashing
!>
!> Implements the FNV 1a algorithm for 32bit hashes
!>
!> Supports hashing of:
!>  - 32bit integers (scalar & 1D array)
!>  - 64bit integers (scalar & 1D array)
!>  - character(*), default kind
!>
!>  The lack of unsigned arithmetic in Fortran means that
!>   64bit arithmetic is needed to perform 32bit hashing.
!>  Hashes are therefore returned as int64.
!>
module fhash_fnv
  use iso_fortran_env, only: int32, int64
  use iso_c_binding, only: c_char
  implicit none

  private
  public :: fnv_1a, hash_string

  !> Starting seed
  integer(int64), parameter :: FNV_OFFSET_32 = 2166136261_int64

  !> Hashing prime
  integer(int64), parameter :: FNV_PRIME_32 = 16777619_int64

  !> Generic interface to perform hashing
  !>
  !> Usage:
  !>```fortran
  !> fnv_1a([seed],input)
  !>```
  !> where `input` is any of the supported types
  interface fnv_1a
    module procedure fnv_1a_char_scalar
    module procedure fnv_1a_char_scalar_seed
    module procedure fnv_1a_int32_scalar
    module procedure fnv_1a_int32_scalar_seed
    module procedure fnv_1a_int32_1d
    module procedure fnv_1a_int32_1d_seed
    module procedure fnv_1a_int64_scalar
    module procedure fnv_1a_int64_scalar_seed
    module procedure fnv_1a_int64_1d
    module procedure fnv_1a_int64_1d_seed
  end interface fnv_1a

contains
    

  !> Hash a single default kind character variable
  pure function fnv_1a_char_scalar(input) result(hash)
    character(*), intent(in) :: input
    integer(int64) :: hash

    hash = fnv_1a(FNV_OFFSET_32,input)

  end function fnv_1a_char_scalar


  !> Hash a character(*) string of default kind 
  pure function fnv_1a_char_scalar_seed(seed, input) result(hash)
    integer(int64), intent(in) :: seed
    character(*), intent(in) :: input
    integer(int64) :: hash

    integer :: i
    integer(int64) :: item

    hash = seed

    do i=1,len(input)
      item = transfer([iachar(input(i:i),int32),0_int32],item)
      hash = ieor(hash,item) * fnv_prime_32
    end do

  end function fnv_1a_char_scalar_seed


  !> Hash a single 32bit integer
  pure function fnv_1a_int32_scalar(input) result(hash)
    integer(int32), intent(in) :: input
    integer(int64) :: hash

    hash =  fnv_1a(FNV_OFFSET_32,input)

  end function fnv_1a_int32_scalar


  !> Hash a single 32bit integer with a starting seed
  pure function fnv_1a_int32_scalar_seed(seed,input) result(hash)
    integer(int64), intent(in) :: seed
    integer(int32), intent(in) :: input
    integer(int64) :: hash

    character(len=4,kind=c_char) :: chars

    chars = transfer(input,chars)

    hash =  fnv_1a(seed,chars)

  end function fnv_1a_int32_scalar_seed


  !> Hash a 1D array of 32bit integers
  pure function fnv_1a_int32_1d(input) result(hash)
    integer(int32), intent(in) :: input(:)
    integer(int64) :: hash

    hash =  fnv_1a(FNV_OFFSET_32,input)

  end function fnv_1a_int32_1d


  !> Hash a 1D array of 32bit integers with a starting seed
  pure function fnv_1a_int32_1d_seed(seed,input) result(hash)
    integer(int64), intent(in) :: seed
    integer(int32), intent(in) :: input(:)
    integer(int64) :: hash

    integer :: i

    hash = seed
    do i=1,size(input)
      hash = fnv_1a(hash,input(i))
    end do

  end function fnv_1a_int32_1d_seed


  !> Hash a single 64bit integer
  pure function fnv_1a_int64_scalar(input) result(hash)
    integer(int64), intent(in) :: input
    integer(int64) :: hash

    hash =  fnv_1a(FNV_OFFSET_32,input)

  end function fnv_1a_int64_scalar


  !> Hash a single 64bit integer with a starting seed
  pure function fnv_1a_int64_scalar_seed(seed,input) result(hash)
    integer(int64), intent(in) :: seed
    integer(int64), intent(in) :: input
    integer(int64) :: hash

    character(len=8,kind=c_char) :: chars

    chars = transfer(input,chars)

    hash =  fnv_1a(seed,chars)

  end function fnv_1a_int64_scalar_seed


  !> Hash a 1D array of 64bit integers
  pure function fnv_1a_int64_1d(input) result(hash)
    integer(int64), intent(in) :: input(:)
    integer(int64) :: hash

    hash =  fnv_1a(FNV_OFFSET_32,input)

  end function fnv_1a_int64_1d


  !> Hash a 1D array of 64bit integers with a starting seed
  pure function fnv_1a_int64_1d_seed(seed,input) result(hash)
    integer(int64), intent(in) :: seed
    integer(int64), intent(in) :: input(:)
    integer(int64) :: hash

    integer :: i

    hash = seed
    do i=1,size(input)
      hash = fnv_1a(hash,input(i))
    end do

  end function fnv_1a_int64_1d_seed


  !> Help fcn to convert hash to hex representation
  function hash_string(hash_value) result(str)
    integer(int64), intent(in) :: hash_value
    character(:), allocatable :: str

    allocate(character(len=10) :: str)
    write(str,'(Z0)') int(hash_value,int32)

  end function hash_string

  
end module fhash_fnv