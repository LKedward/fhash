---
title: User Guide
---

## Basic usage

The package provides a `[[fhash_tbl_t]]` type with `[[fhash_tbl_t(type):set(bound)]]` and `[[fhash_tbl_t(type):get(bound)]]` methods for storing and retrieving key-value pairs.

__e.g.__ *Declare a table instance*

```fortran
use fhash
...
type(fhash_tbl_t) :: tbl
```


### The `fhash_key` interface

A extensible interface for generating an instance of `[[fhash_key_t]]` needed to set and get key-value pairs.

The library provides generic support for generating keys from scalar or 1D arrays of integers (`integer(int32)`, `integer(int64)`) or from scalar `character(*)`

__e.g.__ *Create a key from a string*

```fortran
use fhash, only: key=>fhash_key
...
key('key_string')
```

__For how to extend the `[[fhash_key_t]]` interface to other types, see the [custom key demo](|url|/page/3-custom-key-demo/index.html)__




### The `[[fhash_tbl_t(type):set(bound)]]` method

Stores (or overwrites) a key-value pair into the hash table.

Will accept __any scalar variable__ as a value, including (scalar) derived types.

__e.g.__ *Set key value pair*

```fortran
use fhash, only: fhash_tbl_t, key=>fhash_key
...
type(fhash_tbl_t) :: tbl

tbl%set(key('key'),value=1)
```

To store a pointer instead of copying the value, use `[[fhash_tbl_t(type):set_ptr(bound)]]`.


### The `[[fhash_tbl_t(type):get(bound)]]` method

A generic interface for __retrieving intrinsic scalar__ values from the table.

The library provides generic support for retrieving scalar values of the following types:

- `integer(int32)`, `integer(int64)`
- `real(real32)`, `real(real64)`
- `character(*)`
- `logical`

__e.g.__ *Get integer value for a key*

```fortran
use fhash, only: fhash_tbl_t, key=>fhash_key
...
type(fhash_tbl_t) :: tbl
integer :: v

tbl%get(key('key'),value=v)
```

An optional integer `stat` argument may be passed which is non-zero on exit if the retrieval was unsuccessful. Non-zero return values may be one of: `FHASH_KEY_NOT_FOUND`, `FHASH_FOUND_WRONG_TYPE`, or `FHASH_EMPTY_TABLE`.

__For how to retrieve derived types see the [derived type demo](|url|/page/2-derived-type-demo/index.html).__

To retrieve a pointer instead of copying the value, use `[[fhash_tbl_t(type):get_ptr(bound)]]`.


### A simple example program
```fortran
{!app/0-simple-demo/main.f90!}
```
