module test_fnv
  use iso_fortran_env, only: int32, int64
  use TestLite_suite, only : new_unittest, unittest_t, error_t, test_failed
  use fhash_fnv
  implicit none

  private
  public collect_fnv

  contains

  !> Collect all exported unit tests
  subroutine collect_fnv(testsuite)

    !> Collection of tests
    type(unittest_t), allocatable, intent(out) :: testsuite(:)
    
    testsuite = [ &
        & new_unittest("string-hashing", test_string_hashing), &
        & new_unittest("string-hash-with-seed", test_string_hash_with_seed), &
        & new_unittest("int32-hashing", test_int32_hashing), &
        & new_unittest("int32-array-hashing", test_int32_array_hashing), &
        & new_unittest("int64-hashing", test_int64_hashing), &
        & new_unittest("int64-array-hashing", test_int64_array_hashing) &
        ]
        
  end subroutine collect_fnv


  !> Some simple string hashes checked against https://md5calc.com/hash
  subroutine test_string_hashing(error)
    type(error_t), allocatable, intent(out) :: error

    call check_hash('a','E40C292C')
    if (allocated(error)) return

    call check_hash('z','FF0C53AD')
    if (allocated(error)) return

    call check_hash('ab','4D2505CA')
    if (allocated(error)) return

    call check_hash('abc','1A47E90B')
    if (allocated(error)) return

    call check_hash('abcdefghijklmnopqrstuvwxyz','B0BC0C82')
    if (allocated(error)) return

    call check_hash('ABCDEFGHIJKLMNOPQRSTUVWXYZ','8A88DD82')
    if (allocated(error)) return

    call check_hash('0123456789','F9808FF2')
    if (allocated(error)) return

    call check_hash('`¬!"£$%^&*()_+-=[]{};''#:@~,./<>? ','62BB758C')
    if (allocated(error)) return

    call check_hash('A much longer string to hash;'// &
                    ' look, it continues onto the next line.'// &
                    ' And again onto another line!'// &
                    ' I can do this all day!'//&
                    ' I can do this all day!'//&
                    ' I can do this all day!','FA05C37E')
    if (allocated(error)) return

    contains

    subroutine check_hash(string,hash)
      character(*), intent(in) :: string
      character(*), intent(in) :: hash

      character(:), allocatable :: actual
      actual = hash_string(fnv_1a(string))

      if (trim(actual) /= hash) then

        allocate(error)
        error%message = 'String hash check failed for string "'//string//'"'//new_line('a')//&
                    ' expected "'//trim(hash)//'" but got "'//trim(actual)//'"'

        return

      end if

    end subroutine check_hash

  end subroutine test_string_hashing


  !> Check custom hashing of character strings
  subroutine test_string_hash_with_seed(error)
    type(error_t), allocatable, intent(out) :: error

    integer(int64) :: hash1, hash2

    hash1 = fnv_1a('a')
    hash1 = fnv_1a(hash1,'b')

    hash2 = fnv_1a('ab')

    if (hash1 /= hash2) then
      allocate(error)
      error%message = 'Custom hash with seed failed.'
    end if

  end subroutine test_string_hash_with_seed

  
  !> Check for int32 hash regression
  subroutine test_int32_hashing(error)
    type(error_t), allocatable, intent(out) :: error

    call check_hash(int(0,int32),'4B95F515')
    if (allocated(error)) return

    call check_hash(int(1,int32),'FB69B604')
    if (allocated(error)) return

    call check_hash(int(-1,int32),'E3160FB1')
    if (allocated(error)) return

    call check_hash(huge(int(1,int32)),'6316D931')
    if (allocated(error)) return

    call check_hash(-1*huge(int(1,int32)),'7B6A7F84')
    if (allocated(error)) return
    
    contains

    subroutine check_hash(input,hash)
      integer(int32), intent(in) :: input
      character(*), intent(in) :: hash

      character(50) :: int_string

      character(:), allocatable :: actual
      actual = hash_string(fnv_1a(input))

      if (trim(actual) /= hash) then

        write(int_string,*) input
        allocate(error)
        error%message = 'String hash check failed for 32bit integer "'//trim(int_string)//'"'//new_line('a')//&
                    ' expected "'//trim(hash)//'" but got "'//trim(actual)//'"'

        return

      end if

    end subroutine check_hash

  end subroutine test_int32_hashing


  !> Check for int32 array hash regression
  subroutine test_int32_array_hashing(error)
    type(error_t), allocatable, intent(out) :: error

    integer :: i

    call check_hash(int([0,1,2,3,4,5],int32),'40964454')
    if (allocated(error)) return

    call check_hash(int([(i,i=100,1000,100)],int32),'2C6E00E3')
    if (allocated(error)) return
    
    call check_hash(int([(i,i=-10000,10000,1000)],int32),'945FF111')
    if (allocated(error)) return

    contains

    subroutine check_hash(input,hash)
      integer(int32), intent(in) :: input(:)
      character(*), intent(in) :: hash

      character(:), allocatable :: actual
      actual = hash_string(fnv_1a(input))

      if (trim(actual) /= hash) then

        allocate(error)
        error%message = 'String hash check failed for 32bit integer array'//new_line('a')//&
                    ' expected "'//trim(hash)//'" but got "'//trim(actual)//'"'

        return

      end if

    end subroutine check_hash

  end subroutine test_int32_array_hashing


  !> Check for int64 hash regression
  subroutine test_int64_hashing(error)
    type(error_t), allocatable, intent(out) :: error

    call check_hash(int(0,int64),'9BE17165')
    if (allocated(error)) return

    call check_hash(int(1,int64),'3E801244')
    if (allocated(error)) return

    call check_hash(int(-1,int64),'6CAE0A5D')
    if (allocated(error)) return

    call check_hash(int(huge(int(1,int32)),int64),'D8637B41')
    if (allocated(error)) return

    call check_hash(int(-1*huge(int(1,int32)),int64),'2AD47C60')
    if (allocated(error)) return
    
    call check_hash(huge(int(1,int64)),'ECAED3DD')
    if (allocated(error)) return

    call check_hash(-1*huge(int(1,int64)),'BE80DBC4')
    if (allocated(error)) return

    contains

    subroutine check_hash(input,hash)
      integer(int64), intent(in) :: input
      character(*), intent(in) :: hash

      character(50) :: int_string

      character(:), allocatable :: actual
      actual = hash_string(fnv_1a(input))

      if (trim(actual) /= hash) then

        write(int_string,'(I0)') input
        allocate(error)
        error%message = 'String hash check failed for 64bit integer "'//trim(int_string)//'"'//new_line('a')//&
                    ' expected "'//trim(hash)//'" but got "'//trim(actual)//'"'

        return

      end if

    end subroutine check_hash

  end subroutine test_int64_hashing


  !> Check for int64 array hash regression
  subroutine test_int64_array_hashing(error)
    type(error_t), allocatable, intent(out) :: error

    integer :: i

    call check_hash(int([0,1,2,3,4,5],int64),'67C5CFE4')
    if (allocated(error)) return

    call check_hash(int([(i,i=100,1000,100)],int64),'83CE0AD3')
    if (allocated(error)) return
    
    call check_hash(int([(i,i=-10000,10000,1000)],int64),'BC95B561')
    if (allocated(error)) return

    contains

    subroutine check_hash(input,hash)
      integer(int64), intent(in) :: input(:)
      character(*), intent(in) :: hash

      character(:), allocatable :: actual
      actual = hash_string(fnv_1a(input))

      if (trim(actual) /= hash) then

        allocate(error)
        error%message = 'String hash check failed for 32bit integer array'//new_line('a')//&
                    ' expected "'//trim(hash)//'" but got "'//trim(actual)//'"'

        return

      end if

    end subroutine check_hash

  end subroutine test_int64_array_hashing

end module test_fnv