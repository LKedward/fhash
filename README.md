# fhash
__[fpm](https://github.com/fortran-lang/fpm) package implementing a hash table with support for generic keys and values.__

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![fpm test](https://github.com/LKedward/fhash/workflows/fpm%20test/badge.svg?branch=master&event=push)](https://github.com/LKedward/fhash/actions)
[![ford docs](https://img.shields.io/badge/FORD%20API%20Docs-Deployed-green)](https://lkedward.github.io/fhash/)



## Simple example: scalar intrinsics

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


## Advanced usage

- [Storing custom derived types as values](./app/derived_type_demo)
- [Using custom derived types as keys](./app/custom_key_demo)
- Storing and retrieving pointers (*coming soon*)


## Full API Documentation

See <https://lkedward.github.io/fhash/>
