Pins that two digit separators `'` in a row are not a literal: `#xFF''FF` stops at `#xFF`. The compiler used to accept any run of separators after the first digit (issue #47).
