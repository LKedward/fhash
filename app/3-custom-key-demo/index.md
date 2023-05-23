---
title: Example: Custom key types
---

## Example: Custom Key Types

Custom key types can be defined simply by extension of the abstract `fhash_key_t` type
defined in [`fhash_key_base`](../../src/fhash_key/base.f90).
Extensions of this type must implement the `equals`, `hash` and `to_string` procedures.
Optionally, you may also override the `fhash_key` interface with a helper function to
generate a key from your custom type.

To perform the hashing, the included `fhash_fnv` module provides the `fnv_1a` interface
which supports default scalar characters and 32bit/64bit scalar/1D integers.
You can use this interface to generate a hash from the components of your derived type.

In this example, a `key_string_t` key container type is defined as an extension
of `fhash_key_T` which allows the `string_t` derived type to be used as a key.

```fortran
{!app/3-custom-key-demo/main.f90!}
```
