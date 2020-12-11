---
title: Example: Custom value types
---

## Example: Custom value types

Since an unlimited polymorphic argument is used when storing hash table
values, no modification is required to set custom derived types as values.

However, when retrieving values it is convenient to define a custom getter
routine to correctly obtain the derived type value from the table.

In this demo, a custom getter is defined for a `string_t` type.
The getter must obtain a raw polymorphic allocatable from the hash table,
and determine the type using the `select type` construct. The getter 
should therefore implement some form of error handling for the case when
the requested key is not of the expected derived type.

```fortran
{!app/2-derived-type-demo/main.f90!}
```
