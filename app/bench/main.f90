!> Benchmark program
program fhash_demo
  use fhash, only: fhash_tbl_t, key=>fhash_key
  implicit none

  type(fhash_tbl_t) :: tbl
  integer :: i, stat
  integer :: num_buckets, num_items, num_collisions, max_depth
  
  integer, parameter :: n = 1e7
  real(kind(0.d0)) :: t0, t1

  print *, '# fhash demo program: benchmark'

  !> Manually specify number of table buckets
  call tbl%allocate(2*n)

  print *, 'Setting keys... '
  call cpu_time(t0)
  do i=1,n

    call tbl%set(key([1,2,3,4,5,6,7,8,9,10,11,12,13,i]),0.0d0)

  end do
  call cpu_time(t1)

  print *, 'Time: ', (t1-t0)
  print *, 'Items per second: ', n/(t1-t0)
  print *
  
  !> Query information about the hash table
  print *, 'Querying table info...'
  call tbl%stats(num_buckets,num_items,num_collisions,max_depth)
  write(*,'(A,T40,I0)') '  Number of buckets allocated: ',num_buckets
  write(*,'(A,T40,I0)') '  Number of key-value pairs stored: ',num_items
  write(*,'(A,T40,I0)') '  Total number of hash-collisions: ',num_collisions
  write(*,'(A,T40,I0)') '  The worst case bucket depth is ',max_depth
  print *
  
end program fhash_demo