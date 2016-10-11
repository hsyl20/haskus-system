# ViperVM: memory management

## Memory Pointers

A pointer is a number: an offset into a memory. This is the ```Addr#``` type.

We want the type-system to help us avoid errors when we use pointers, hence we
decorate them with phantom types describing the memory layout at the pointed
address. This is the ```Ptr a``` data type that wraps an ```Addr#```.

We often want to associate finalizers to pointers, i.e., actions to be run when
the pointer is collected by the GC. These actions take the pointer as a
parameter. This is the ```ForeignPtr a``` data type.

A ```ForeignPtr a``` cannot be manipulated like a number because somehow we need
to keep the pointer value that will be passed to the finalizers. Moreover we
don't want finalizers to be executed too early, so we can't easily create a new
```ForeignPtr``` from another (it would require a way to disable the existing
finalizers of a ```ForeignPtr```, which would in turn open a whole can of
worms). Hence we use the ```FinalizedPtr a``` pointer type, which has an
additional offset field.

The ```PtrLike``` class is used to abstract over the two main pointer types:
```Ptr a``` and ```FinalizedPtr a```.

## Memory Layouts

Memory layouts are types that describe a memory region. They are used as
phantoms types for pointers. 

Examples of memory layouts:
* Vector n e: vector of 'n' elements of type 'e'
* Struct fs: structure/record with named fields, respecting alignment of the
  fields by adding padding bytes
* PackedStruct fs: structure/record with named fields, without additional
  padding bytes
* Union ls: overlaid layouts (i.e., layouts over the same memory region)
* Rect w h p e: rectangular region with padding bytes between each row

## Storable Data Types

It is possible to associate a representation in memory to some data types: these
data types can then be read from memory or written into it. These data types can
also be used as pointer phantom types.


