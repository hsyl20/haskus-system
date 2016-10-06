# ViperVM: type-safe memory

Haskell supports raw pointer manipulation (see Foreign.Ptr) and data read/write
(peek/poke) into memory (see Foreign.Storable). In ViperVM, we want to do that
in a safer way while remaining as fast as possible. In order to do that, we will
mimic C pointers. 

## C pointers

In C, a pointer with type ```X *``` points to a memory region with the ```X```
layout. X can be a struct, an enum, an union, a primitive data-type, an array,
etc.

C pointers are then dereferenced using either:

* ```*``` dereferences a primitive
* ```[n]``` dereferences an array element
* ```->symbol``` dereferences a struct element

Dereference addresses are computed at compile time.
