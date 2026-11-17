Pins that a `ByteSize` literal has the range of a `size_t`, like `Index`: 256 and 2^64 - 1 fit. The compiler used to give `ByteSize` the bounds of 8 bits, 0 to 255 (issues #144 and #163).
