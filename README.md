# fhash
__[fpm](https://github.com/fortran-lang/fpm) package implementing a hash table with support for generic keys and values.__

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![fpm test](https://github.com/LKedward/fhash/workflows/fpm%20test/badge.svg?branch=master&event=push)](https://github.com/LKedward/fhash/actions)
[![ford docs](https://img.shields.io/badge/FORD%20API%20Docs-Deployed-green)](https://lkedward.github.io/fhash/)



## fpm usage

To use *fhash* within your *fpm* project, add the following to your package manifest file (`fpm.toml`):

```toml
[dependencies]
fhash = { git = "https://github.com/LKedward/fhash.git" }
```


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

See the [Quickstart Guide](https://lkedward.github.io/fhash/page/index.html) for an explanation of this example and the API methods used.


## Advanced usage

- [More table methods](https://lkedward.github.io/fhash/page/1-methods-demo/index.html)
- [Storing custom derived types as values](https://lkedward.github.io/fhash/page/2-derived-type-demo/index.html)
- [Using custom derived types as keys](https://lkedward.github.io/fhash/page/3-custom-key-demo/index.html)


See <https://lkedward.github.io/fhash/> for the full API documentation.