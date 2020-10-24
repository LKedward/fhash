## fhash demo: custom key types

Custom key types can be defined simply by extension of the abstract `fhash_key_t` type
defined in [fhash_key_base](../src/fhash_key/base.f90).
Extensions of this type must implement the `equals` and `hash` procedures.
Optionally, you may also override the `fhash_key` interface with a helper
function to generate a key from your custom type.


NB. Elementary hash function `fnv_1a` provides support for
default scalar characters and 32bit/64bit scalar and 1D integers

