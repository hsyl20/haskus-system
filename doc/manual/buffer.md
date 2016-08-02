# Buffer

A buffer is a sequence of bytes. It generally consists in a pointer and a
size. There are many different use cases depending on:

* how they are allocated and where:
   * outside GHC's heap: malloc, mmap
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

* strict bytestring
   * memory allocated in GHC's heap (pinned!)
   * subject to heap fragmentation
* Fields:
   * ForeignPtr: support finalizers in theory, in practice bytestring doesn't use them
   * Offset: sharing, O(1) slicing (Int...)
   * Length: runtime length (Int...)

* lazy bytestring
   * linked-list of strict bytestring

* plain bytestring
   * smaller overhead
   * wrapper around MutableByteArray#
   * still pinned

* short bytestring
   * smaller overhead
   * wrapper around ByteArray#
   * unpinned

## References

* http://meiersi.github.io/HaskellerZ/meetups/2012%2001%2019%20-%20The%20bytestring%20library/handout.html
* http://stackoverflow.com/questions/4908880/is-there-any-hope-to-cast-foreignptr-to-bytearray-for-a-function-bytestring
