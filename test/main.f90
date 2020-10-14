program test
use iso_fortran_env, only: int32, int64
use errortest, only: testsuite_t, new_testsuite, run_tests
use test_fnv, only: collect_fnv
use test_key, only: collect_key
use test_container, only: collect_container
use test_sll, only: collect_sll
use test_tbl, only: collect_tbl
use fhash
use fhash_sll
! use fhash_key_char
! use fhash_key_int32
implicit none

type(fhash_tbl_t) :: tbl
integer :: i,j, stat
integer, allocatable :: v

type(fhash_key_char_t) :: my_key1, my_key2

! integer :: stat, is
    ! character(len=:), allocatable :: suite_name, test_name
    type(testsuite_t), allocatable :: testsuite(:)
    ! character(len=*), parameter :: fmt = '("#", *(1x, a))'


    

    testsuite = [ &
        & new_testsuite("fhash_fnv", collect_fnv), &
        & new_testsuite("fhash_key", collect_key), &
        & new_testsuite("fhash_container", collect_container), &
        & new_testsuite("fhash_sll", collect_sll), &
        & new_testsuite("fhash_tbl", collect_tbl) &
    ]

    call run_tests(testsuite)

    ! write(*,'(Z0)') int(255,int32)

my_key1 = fhash_key('asdf')
my_key2 = fhash_key('asdf')

print *, my_key1%hash()
print *, my_key1 == my_key2

! print *, fnv_1a('This is a very long sentence to hash with fnv_1a.'//&
!                 'As you can see, the sentence continues on another line.'//&
!                 'And another line continuation!'//&
!                 'And another line continuation!'//&
!                 'And another line continuation!'//&
!                 'And another line continuation!'//&
!                 'And another line continuation!'//&
!                 'And another line continuation!'//&
!                 'And another line continuation!')


! write(*,'(Z0)') fnv_1a('aaaa')
! write(*,'(Z0)') int(fnv_1a('A medium sentence to hash. '),int32)

! a = 2166136261_int64

! a = fnv_1a(iachar('a'))
! a = fnv_1a(a,iachar('b'))

! write(*,'(Z0)') modulo(fnv_1a('a'),128)
! write(*,'(Z0)') modulo(fnv_1a('ab'),128)
! write(*,'(Z0)') modulo(fnv_1a('ac'),128)

! call tbl%allocate(127)
do i=0,25
    do j=0,25
        call tbl%set(fhash_key(char(iachar('a')+i)//char(iachar('a')+j)),i*j)
    end do
end do

call tbl%set(fhash_key(i*j),i*j+1)

! print *,i*j,tbl%get_int32(fhash_key(i*j))

! do i=0,25
!   do j=0,25
!       ! call fhash_tbl_get(tbl,new_key(char(iachar('a')+i)//char(iachar('a')+j)),v,stat)
!       ! v = tbl%get_int32(fhash_key(char(iachar('a')+i)//char(iachar('a')+j)),stat)
!       if (.not.allocated(V)) then
!         write(*,*) 'not found', char(iachar('a')+i)//char(iachar('a')+j), stat
!         cycle
!       end if
!       ! select type(v)
!         ! type is(integer)
!           if (i*j /= v) then
!             write(*,*) 'wrong value', i*j,v,stat
!           end if
!         ! class default
!           ! write(*,*) 'wrong type returned'
!       ! end select
!   end do
! end do

print *, maxval([(node_depth(tbl%buckets(i)),i=1,67)])





end program test