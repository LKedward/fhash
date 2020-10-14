# fhash (_beta_)
__[fpm](https://github.com/fortran-lang/fpm) package implementing a hash table with support for generic keys and values.__

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![fpm test](https://github.com/LKedward/fhash/workflows/fpm%20test/badge.svg?branch=master&event=push)](https://github.com/LKedward/fhash/actions)

## Examples

### Scalar intrinsics

The package provides a `fhash_tbl_t` type with `set` and `get` methods for storing and retrieving key-value pairs.
The `fhash_key` interface (aliased to `key` below) is used to define a valid key from different inputs.

```fortran
program fhash_demo1
  use fhash, only: fhash_tbl_t, key=>fhash_key
  implicit none
  type(fhash_tbl_t) :: tbl
  integer :: val
  
  call tbl%set(key('my_key_1'), value=10)
  call tbl%set(key('my_key_2'), value=1.0)
  call tbl%set(key(123456), value='a string value')
  call tbl%set(key([1,2,3,4,5]), value=.false.)
  
  call tbl%get(key('my_key_1'),val)
  
end program fhash_demo1
```

- The `set` method will accept any scalar variable as a value including derived types

- The `get` method has generic support for retrieving scalar values of the following types: `integer(int32)`, `integer(int64)`, `real(real32)`, `real(real64)`, `character(*)`, `logical`

- `fhash_key` has generic support for generating keys from scalar or 1D arrays of integers (`integer(int32)`, `integer(int64)`) or from scalar `character(*)`


### Derived types as values

Storage of scalar custom derived types in the hash table only requires the definition of a custom getter routine.

__Example:__

```fortran
program fhash_demo2
  use fhash, only: fhash_tbl_t, key=>fhash_key, fhash_key_t
  implicit none

  type string_t
    character(:), allocatable :: s
  end type string_t
  
  type(fhash_tbl_t) :: tbl
  type(string_t) :: str1, str2
  
  str1%s = 'Hello fhash'
  call tbl%set(key('key_1'), value=str1)
  
  call fhash_get_string(tbl,key('key_1'),str2)

  contains
  
  !> Define custom getter for string_t type
  subroutine fhash_get_string(tbl,k,string)
    type(fhash_tbl_t), intent(in) :: tbl
    class(fhash_key_t), intent(in) :: k
    type(string_t), intent(out) :: string
    
    integer :: stat
    class(*), allocatable :: data
    
    call tbl%get(k,data,stat)
    
    if (stat /= 0) ! Error handling: key not found

    select type(d=>data)
    type is (string_t)
      string = d
    class default
      ! Error handling: wrong type found
    end select
    
  end subroutine fhash_get_string
  
end program fhash_demo2
```


### Derived types as keys

Derived types can be used as keys by overloading the `fhash_key` interface to accept the derived type and return a concrete instance of `fhash_key_t`.

__Example:__ Coming soon.


### Storing and retrieving pointers

__Example:__ Coming soon.
