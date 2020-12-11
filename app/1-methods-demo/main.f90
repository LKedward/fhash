!> Example program demonstrating useful
!>  fhash_tbl_t methods 
program fhash_demo
  use fhash, only: fhash_tbl_t, key=>fhash_key
  implicit none

  type(fhash_tbl_t) :: tbl
  integer :: i, stat
  integer :: num_buckets, num_items, num_collisions, max_depth

  print *, '# fhash demo program: methods-demo'

  !> Manually specify number of table buckets
  call tbl%allocate(301)

  print *, 'Setting keys... '
  do i=0,256

    call tbl%set(key(i),i*i)

  end do
  
  !> Query information about the hash table
  print *, 'Querying table info...'
  call tbl%stats(num_buckets,num_items,num_collisions,max_depth)
  write(*,'(A,T40,I0)') '  Number of buckets allocated: ',num_buckets
  write(*,'(A,T40,I0)') '  Number of key-value pairs stored: ',num_items
  write(*,'(A,T40,I0)') '  Total number of hash-collisions: ',num_collisions
  write(*,'(A,T40,I0)') '  The worst case bucket depth is ',max_depth
  print *

  !> Check for existence of a key
  call tbl%check_key(key(0),stat)
  print *, 'Check key 0: ',merge('FOUND    ','NOT FOUND',stat==0)

  !> Unset a key in the table
  print *, 'Removing key 0...'
  call tbl%unset(key(0))

  call tbl%check_key(key(0),stat)
  print *, 'Check key 0: ',merge('FOUND    ','NOT FOUND',stat==0)
  print *
  
end program fhash_demo