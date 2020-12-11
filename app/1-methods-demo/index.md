---
title: Hash table methods
---

## Hash table methods

Other hash table methods, in addition to `[[fhash_tbl_t(type):set(bound)]]` and `[[fhash_tbl_t(type):get(bound)]]`, are:

- `[[fhash_tbl_t(type):allocate(bound)]]`: manually allocate the number of table buckets

- `[[fhash_tbl_t(type):check_key(bound)]]`: determine whether a given key exists within the table

- `[[fhash_tbl_t(type):unset(bound)]]`: remove a key-value pair from the table

- `[[fhash_tbl_t(type):stats(bound)]]`: query information about the table instance


### Example

```fortran
{!app/1-methods-demo/main.f90!}
```
