module test_key
  use iso_fortran_env, only: int32, int64
  use TestLite_suite, only : new_unittest, unittest_t, error_t, test_failed
  use fhash_fnv, only: hash_string
  use fhash_key_base
  use fhash_key_char
  use fhash_key_int32
  use fhash_key_int64
  use fhash_key_int32_1d
  use fhash_key_int64_1d
  
  implicit none

  private
  public collect_key

  contains

  !> Collect all exported unit tests
  subroutine collect_key(testsuite)

    !> Collection of tests
    type(unittest_t), allocatable, intent(out) :: testsuite(:)
    
    testsuite = [ &
        & new_unittest("key-char", test_key_char), &
        & new_unittest("key-int32", test_key_int32), &
        & new_unittest("key-int32-1d", test_key_int32_1d), &
        & new_unittest("key-int64", test_key_int64), &
        & new_unittest("key-int64-1d", test_key_int64_1d) &
        ]
        
  end subroutine collect_key


  !> Test character(*) hash key implementation
  subroutine test_key_char(error)
    type(error_t), allocatable, intent(out) :: error

    class(fhash_key_t), allocatable :: my_key1

    !> Create key from character(*)
    allocate(my_key1, source = fhash_key('abc'))

    !> Check new key
    select type(k=>my_key1)
    type is (fhash_key_char_t)
      
      if (.not.(k == fhash_key('abc'))) then
        call test_failed(error,'Character key equality test failed.')
        return
      end if

      if (k == fhash_key('a')) then
        call test_failed(error,'Character key inequality test failed.')
        return
      end if

      if (k == fhash_key(int(iachar('a'),int32))) then
        call test_failed(error,'Character key type inequality test failed.')
        return
      end if

    class default

      call test_failed(error, 'Wrong type for new key, expected fhash_key_char_t')
      return

    end select

    !> Check hashing
    if (hash_string(my_key1%hash()) /= '1A47E90B') then
      call test_failed(error, 'Wrong hash for new character key.')
      return
    end if

  end subroutine test_key_char


  !> Test scalar int32 hash key implementation
  subroutine test_key_int32(error)
    type(error_t), allocatable, intent(out) :: error

    class(fhash_key_t), allocatable :: my_key1

    !> Create key from scalar int32
    my_key1 = fhash_key(int(0,int32))

    !> Check new key
    select type(k=>my_key1)
    type is (fhash_key_int32_t)
      
      if (.not.(k == fhash_key(int(0,int32)))) then
        call test_failed(error,'int32 key equality test failed.')
        return
      end if

      if (k == fhash_key(int(1,int32))) then
        call test_failed(error,'int32 key inequality test failed.')
        return
      end if

    class default

      call test_failed(error, 'Wrong type for new key, expected fhash_key_int32_t')
      return

    end select

    !> Check hashing
    if (hash_string(my_key1%hash()) /= '4B95F515') then
      call test_failed(error, 'Wrong hash for new int32 key.')
      return
    end if

  end subroutine test_key_int32


  !> Test 1d int32 array hash key implementation
  subroutine test_key_int32_1d(error)
    type(error_t), allocatable, intent(out) :: error

    class(fhash_key_t), allocatable :: my_key1

    !> Create key from scalar int32
    allocate(my_key1, source = fhash_key(int([0,1,2,3,4,5],int32)))

    !> Check new key
    select type(k=>my_key1)
    type is (fhash_key_int32_1d_t)
      
      if (.not.(k == fhash_key(int([0,1,2,3,4,5],int32)))) then
        call test_failed(error,'int32 1d key equality test failed.')
        return
      end if

      if (k == fhash_key(int([0,1],int32))) then
        call test_failed(error,'int32 1d key inequality test failed.')
        return
      end if

    class default

      call test_failed(error, 'Wrong type for new key, expected fhash_key_int32_1d_t')
      return

    end select

    !> Check hashing
    if (hash_string(my_key1%hash()) /= '40964454') then
      call test_failed(error, 'Wrong hash for new int32 1d key.')
      return
    end if

  end subroutine test_key_int32_1d


  !> Test scalar int64 hash key implementation
  subroutine test_key_int64(error)
    type(error_t), allocatable, intent(out) :: error

    class(fhash_key_t), allocatable :: my_key1

    !> Create key from scalar int64
    my_key1 = fhash_key(int(0,int64))

    !> Check new key
    select type(k=>my_key1)
    type is (fhash_key_int64_t)
      
      if (.not.(k == fhash_key(int(0,int64)))) then
        call test_failed(error,'int64 key equality test failed.')
        return
      end if

      if (k == fhash_key(int(1,int64))) then
        call test_failed(error,'int64 key inequality test failed.')
        return
      end if

    class default

      call test_failed(error, 'Wrong type for new key, expected fhash_key_int64_t')
      return

    end select

    !> Check hashing
    if (hash_string(my_key1%hash()) /= '9BE17165') then
      call test_failed(error, 'Wrong hash for new int64 key.')
      return
    end if

  end subroutine test_key_int64


  !> Test 1d int64 array hash key implementation
  subroutine test_key_int64_1d(error)
    type(error_t), allocatable, intent(out) :: error

    class(fhash_key_t), allocatable :: my_key1

    !> Create key from scalar int64
    allocate(my_key1, source = fhash_key(int([0,1,2,3,4,5],int64)))

    !> Check new key
    select type(k=>my_key1)
    type is (fhash_key_int64_1d_t)
      
      if (.not.(k == fhash_key(int([0,1,2,3,4,5],int64)))) then
        call test_failed(error,'int64 1d key equality test failed.')
        return
      end if

      if (k == fhash_key(int([0,1],int64))) then
        call test_failed(error,'int64 1d key inequality test failed.')
        return
      end if

    class default

      call test_failed(error, 'Wrong type for new key, expected fhash_key_int64_1d_t')
      return

    end select

    !> Check hashing
    if (hash_string(my_key1%hash()) /= '67C5CFE4') then
      call test_failed(error, 'Wrong hash for new int64 1d key.')
      return
    end if

  end subroutine test_key_int64_1d

end module test_key