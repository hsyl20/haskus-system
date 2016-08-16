# Buffer

A buffer is a sequence of bytes. It generally consists in a pointer and a
size. There are many different use cases depending on:

* how they are allocated and where:
   * outside GHC's heap: malloc, mmap, custom allocator
   * in GHC's heap, pinned
   * in GHC's heap, unpinned
* their size:
   * small buffers can be duplicated efficiently while big ones can't
* their interaction with the garbage collector:
   * Finalizers required
   * Can be moved? (pointer shared with external code)
   * Temporary pinning?
* the operations that are supported
   * e.g., slicing with sharing is O(1)
* their overhead
   * Word size to store the runtime buffer size
   * cf ByteString.Short and ByteString.Plain
* mutability
   * Are buffers immutable?
   * can they be frozen?
* alignment constraint
   * the buffer may have an alignment constraint

Sizes
   * Known at compile time
   * Word8
   * Word16
   * WordN

Address
   * Fixed (pinned)
   * Temporarily fixed?
   * Movable: alignment constraint
   * Movable: no constraint

## ByteString

ByteStrings come in different flavors:
* strict (foreign pointer, with offset)
* lazy (a lazy list of strict ByteStrings)
* plain (wrapper around MutableByteArray#, pinned)
* short (wrapper around ByteArray#, unpinned)

### Strict

Strict bytestrings are allocated in pinned GHC's heap and have the following fields:
* ForeignPtr
* Offset: byte offset to add to the pointer
* Length

By having offset and length fields, they support O(1) slicing-with-sharing
operations. They can be considered as a "view" over some contiguous part of a
Plain bytestring.

The ForeignPtr data type is generic: it can point to a buffer outside of
GHC's heap, in which case it supports finalizers.

Glitch: Offset and Length fields have a signed integer type (Int) while it
should be an unsigned one.

### Lazy

Lazy bytestrings are list of 32kB strict bytestrings. They can be used to
manipulate large or unbounded streams of data.

In practice, lazy I/O is discouraged and it is better to use libraries with
stronger garanties than lazy I/O: conduit, pipes, etc.

### Plain and Short

Plain bytestrings (in
[bytestring-plain](https://hackage.haskell.org/package/bytestring-plain)
package) and Short bytestrings have smaller memory and time overheads than strict
bytestrings. The difference between both approaches is that Plain bytestrings
are pinned in GHC's heap, while Short bytestrings aren't.

The memory overhead is reduced because:
* the data type has a single field that can be unpacked: a pointer
   * the size of the bytestring is stored in the data payload
   * there is no stored offset
* there is no support for finalizers
   * ByteArray# and MutableByteArray# primitives are handled by GHC

The time overhead is reduced because:
* there is no indirection pointer

Memory overhead:
* 4 words (32-bit or 64-bit, depending on the architecture)
* word boundary padding at the end (up to 3 or 7 bytes, depending on the
  architecture)


## Buffer

Can we do better than the different ByteString variants?

### Statically known size

If the size is known statically (type literals), we don't have to store it.

If the size is small, we can back the buffer with a Word (e.g., Word32, Word64).


## References

* http://meiersi.github.io/HaskellerZ/meetups/2012%2001%2019%20-%20The%20bytestring%20library/handout.html
* http://stackoverflow.com/questions/4908880/is-there-any-hope-to-cast-foreignptr-to-bytearray-for-a-function-bytestring
